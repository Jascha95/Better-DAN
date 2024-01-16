type level = int

let levels = 
  [|(* general log levels, must be present *)
    ("full-debug", "FD", false);
    ("debug", "D", false);
    ("full-info", "FI", false);
    ("info", "I", true);
    ("warn", "W", true);
    ("error", "E", true);
    ("fatal", "F", true);
    (* application specific log levels, can be adjusted to 
       your needs *)
  |]

let level_of_string s = 
  let rec f i =
    if i >= Array.length levels then None
    else if (let name, _, _ = levels.(i) in name = s)
    then Some i
    else f (i + 1)
  in 
    match f 0 with
        Some i -> i
      | None   ->
          invalid_arg ("Log.get_level_index: Illegal level string: " 
                       ^ s)
let string_of_level i = let (s,_,_) = levels.(i) in s
     
let level_full_debug = level_of_string "full-debug"
let level_debug = level_of_string "debug"
let level_full_info = level_of_string "full-info"
let level_info = level_of_string "info"
let level_warn = level_of_string "warn"
let level_error = level_of_string "error"
let level_fatal = level_of_string "fatal"

let mandatory_level = level_error

let rec disable_level i = 
  if i >= mandatory_level then
    failwith ("Log level '" ^ string_of_level i 
              ^ "' cannot be disabled")
  else
    let name, sname, is_enabled = levels.(i) in
      levels.(i) <- (name, sname, false);
      if i >= 1 then disable_level (i - 1)

let rec enable_level i = 
  let name, sname, is_enabled = levels.(i) in
    levels.(i) <- (name, sname, true);
    if i < Array.length levels - 1 then enable_level (i + 1)

let set_level i = 
  enable_level i;
  if i > 0 then disable_level (i-1) else ()

let with_level i f =
  let old_levels = Array.map (fun (_,_,x) -> x) levels in
  let restore _ = 
    for i = 0 to Array.length levels - 1 do
      let (x, y, _) = levels.(i) in
        levels.(i) <- (x,y,old_levels.(i))
    done
  in
    if i >= 1 then disable_level (i - 1);
    enable_level i;
    try
      let x = f () in
        restore ();
        x
    with exn ->
      restore ();
      raise exn
        
  
let is_enabled i = let _, _, b = levels.(i) in b

let toggle_level i = 
  if is_enabled i then disable_level i
  else enable_level i

let level_names = 
  let first (x, _, _) = x in
    Array.to_list (Array.map first levels)

let level_id i = let (_, y, _) = levels.(i) in y

type log_target = File of out_channel | Term of out_channel

let log_targets = ref [Term stderr]
let change_log_target ch = log_targets := [File ch]
let add_log_target ch =
  log_targets := File ch :: !log_targets

let open_log_file s =
  open_out_gen
    [Open_wronly; Open_append; Open_creat; Open_text] 
    0o644
    s

let format_time t =
  let tm = Unix.localtime t in
  let ms = int_of_float ((mod_float t 1.0) *. 1000.0) in
    Printf.sprintf "%4d-%02d-%02d %02d:%02d:%02d.%03d"
      (tm.Unix.tm_year + 1900)
      (tm.Unix.tm_mon + 1)
      tm.Unix.tm_mday
      tm.Unix.tm_hour
      tm.Unix.tm_min
      tm.Unix.tm_sec
      ms

let use_ansi_esc = ref true
let use_ansi_color = ref true

let disable_ansi_esc _ = use_ansi_esc := false
let disable_ansi_color _ = use_ansi_color := false


let print_ansi_code ch code =
  if !use_ansi_esc then
    (output_char ch (Char.chr 27);
     output_string ch code)
  else ()

let bold_on ch = 
  print_ansi_code ch "[1m"

let bold_off ch = 
  print_ansi_code ch "[22m"

let red_on ch =
  if !use_ansi_color then
    print_ansi_code ch "[31m"
  else
    ()

let all_off ch =
  print_ansi_code ch "[0m"

let render_log level time msg = function
  | (Term ch) ->
      let noact _ = () in
      let (pre,post) = 
        if level = level_full_debug then
          ((fun _ -> output_string ch "[FDEBUG] "), noact)
        else if level = level_debug then
          ((fun _ -> output_string ch "[DEBUG] "), noact)
        else if level = level_full_info then
          (noact, noact)
        else if level = level_info then
          (noact,noact) 
        else if level = level_warn then
          ((fun _ -> output_string ch "WARNING: "), noact)
        else if level = level_error then
          (* error *)
          ((fun _ -> 
              bold_on ch; red_on ch;
              output_string ch (String.uppercase 
                                  (string_of_level level));
              output_string ch ": ";
              all_off ch; bold_on ch;),
           fun _ -> bold_off ch)
        else
          (noact, noact)
      in
        pre ();
        output_string ch msg;
        post ();
        output_char ch '\n';
        flush ch
  | (File ch) ->
      output_char ch '[';
      output_string ch (String.uppercase (string_of_level level));
      output_char ch ' ';
      output_string ch (format_time time);
      output_string ch " pid=";
      output_string ch (string_of_int (Unix.getpid ()));
      output_string ch "] ";
      output_string ch msg;
      output_char ch '\n';
      flush ch

let log level s = 
  let t = Unix.gettimeofday () in
    List.iter (render_log level t s) !log_targets

let full_debug l = 
  if is_enabled level_full_debug
  then 
    log level_debug (Lazy.force l)

let debug l = 
  if is_enabled level_debug
  then 
    log level_debug (Lazy.force l)

let full_info l = 
  if is_enabled level_full_info 
  then log level_full_info (Lazy.force l)

let info l =
  if is_enabled level_info 
  then log level_info (Lazy.force l) 

let warn l =
  if is_enabled level_warn 
  then log level_warn (Lazy.force l)

exception Log_fatal of string

let fatal s = 
  log level_fatal s; 
  raise (Log_fatal s)

let error s =
  log level_error s
