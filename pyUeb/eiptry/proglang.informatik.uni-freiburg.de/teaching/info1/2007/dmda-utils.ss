;; Version history:
;; 2007-12-18: Initial version
;;
;; Author: Stefan Wehr (wehr AT informatik DOT uni MINUS freiburg DOT de)

(module dmda-utils mzscheme
  
  (provide
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; File utilities
    ;;
    ;; Most procedures are document in Section 6.6 of the "Revised^5
    ;; Report on the Algorithmic Language Scheme" (see Helpdesk in
    ;; DrScheme)

    ; Takes a string naming an existing file and returns an input port
    ; capable of delivering characters from the file.
    ; open-input-file: string -> in-port
    open-input-file

    ; Returns the next character available from the input port,
    ; updating the port to point to the following character. If no
    ; more characters are available, an end of file object is
    ; returned.
    ; read-char: in-port -> char
    read-char

    ; Returns a string containing the next line from in-port.
    ; read-line: in-port -> string
    read-line

    ; Returns a string containing the next number characters from
    ; the in-port.
    ; read-string: number in-port -> string
    read-string

    ; Returns #t if obj is an end of file object, otherwise returns
    ; #f.
    ; eof-object?: value-bool
    eof-object?

    ; Closes the file associated with port, rendering the port
    ; incapable of delivering characters.
    ; close-input-port: in-port -> unspecific
    close-input-port

    ; Takes a string naming an output file to be created and returns an
    ; output port capable of writing characters to a new file by that name.
    ; open-output-file: string -> out-port
    (rename open-output-file* open-output-file)

    ; Writes the character to the given port.
    ; write-char: char out-port -> unspecific
    write-char

    ; Writes the string to the given port.
    ; write-string*: string out-port -> unspecific
    write-string*

    ; Closes the file associated with port, rendering the port
    ; incapable of accepting characters.
    ; close-output-port: out-port -> unspecific
    close-output-port

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; String utilities
    ;;
    ;; See Section 6.3.5 of the "Revised^5 Report on the Algorithmic
    ;; Language Scheme"

    ; (string-ref string k) returns character k of string using
    ; zero-origin indexing.
    ; string-ref: string number -> char
    string-ref

    ; (substring string start end) returns a newly allocated string
    ; formed from the characters of string beginning with index start
    ; (inclusive) and ending with index end (exclusive).
    ; substring: string number number -> string
    substring

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; Character utilities
    ;;
    ;; See Section 6.3.4 of the "Revised^5 Report on the Algorithmic
    ;; Language Scheme"

    ; Returns #t if the given value is a character, otherwise returns
    ; #f.
    ; char?: value -> bool
    char?

    ; These predictes impose a total ordering on the set of
    ; characters.
    
    ; char=?: char char -> bool
    char=?

    ; char<?: char char -> bool    
    char<?

    ; char>?: char char -> bool    
    char>?

    ; char<=?: char char -> bool    
    char<=?

    ; char>=?: char char -> bool    
    char>=?

    ; These predicates return #t if their arguments are alphabetic,
    ; numeric, whitespace, upper case, or lower case characters,
    ; respectively, otherwise they return #f.

    ; char-alphabetic?: char -> bool
    char-alphabetic?

    ; char-numeric?: char -> bool
    char-numeric?

    ; char-whitespace?: char -> bool
    char-whitespace?

    ; char-upper-case?: char -> bool
    char-upper-case?

    ; char-lower-case?: char -> bool
    char-lower-case?
  )

  (define (open-output-file* fname)
    (open-output-file fname 'replace))

  (define write-string* write-string)
)