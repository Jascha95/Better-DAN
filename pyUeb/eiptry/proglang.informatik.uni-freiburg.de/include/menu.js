var GLOBAL_MENU_72a2692 = { "menu_prefix" : "MENU_ITEM_",
                            "MENU_MAX_IDS" : 100,
                            "cookie" : {} }

// create a Cookie for the domain
GLOBAL_MENU_72a2692.cookie.setCookie =
function setCookie(c_name,value,expiredays) {
    var exdate=new Date()
    exdate.setDate(exdate.getDate()+expiredays)
    document.cookie = 
        c_name + "=" 
      + escape(value)
      + ((expiredays==null) ? "" 
         : ";expires=" + exdate.toGMTString())
      + ";path=\"/\"";
};

// reads the value of a cookie
GLOBAL_MENU_72a2692.cookie.getCookie = 
function getCookie(c_name) {
  if (document.cookie.length > 0) {
    c_start = document.cookie.indexOf(c_name + "=");
    if (c_start!=-1) { 
      c_start = c_start + c_name.length + 1; 
      c_end = document.cookie.indexOf(";",c_start);
      if (c_end==-1) c_end = document.cookie.length;
      return unescape(document.cookie.substring(c_start,c_end));
    } 
  }
  return "";
}
GLOBAL_MENU_72a2692.cookie.to_cookie = 
function to_cookie() {
  var str = "";
  for (var pn in this) {
    if ((typeof this[pn] == "boolean") && (this[pn] == true))
      str = str + "|" + pn;
  }
  this.setCookie ( this.cookie_name, str, 7)
}
GLOBAL_MENU_72a2692.cookie.addId = 
function addId ( id ) {
  this[id] = true;
  this.to_cookie();
}
GLOBAL_MENU_72a2692.cookie.removeId = 
function addId ( id ) {
  this[id] = false;
  this.to_cookie();
}

GLOBAL_MENU_72a2692.menu_id_to_elm = function ( id ) {
  var e = document.getElementById(this.menu_prefix+id);
  if (e) {
    return e;
  } else {
    return null;
  }
}
GLOBAL_MENU_72a2692.menu_elm_to_id = function ( elm ) {
  if (elm) {
    var id = elm.id;
    if (id) {
      var id = id.substr(10,elm.id.length-10);
      if (id.length > 0)
        return id
        else 
          return "";
    } else {
      return "";
    }
  } else {
    return "";
  }
}

// check if e is an ancestor node of x
GLOBAL_MENU_72a2692.isAncestor = function isAncestor (e, x) {
 for (var scan = e && x; scan; scan = scan.parentNode) {
   if (e == scan)
     break;
 }
 return scan;
} 

/* don't change display style of div's, a's, body oder html 
   tags. Only change style from UL elements, that are items of
   the submenu class. We have to write e["class"], because e.class
   create the keyword class. */
GLOBAL_MENU_72a2692.set_display = function set_display(e,value) {
  if (e) {
    if ((e.nodeName.toUpperCase() == "UL")) {
      e.style.display=value;
    }
  }
}

/* hides the element */
GLOBAL_MENU_72a2692.hide_elm = function hide_elm(e) {
  this.set_display(e,"none");
  var id = e.id.substr(10,e.id.length-10);
  if (id.length > 0) {
    this.cookie.removeId(id);
  }  
}

/* show an element and go up the parentNode Pointers and
   show also all parents. After that step, the element
   is visible, even if parents was hiden bevor.*/
GLOBAL_MENU_72a2692.show_elm = function show_elm(e) {
  if ((e) && (e.style)) {
    this.set_display(e,"block");
    var id = this.menu_elm_to_id (e);
    if (id.length > 0) {
      this.cookie.addId(id);
    }
    if (e.parentNode) {
      this.show_elm(e.parentNode);
    }
  }
}

/* hides all menu items, but don't tough all DOM Nodes,
   that contents elem as descandant */
GLOBAL_MENU_72a2692.hide_all_but = function hide_all_but(elem) {
  for (var i = 0; i<=this.MENU_MAX_IDS; i++) {
    var e = document.getElementById(this.menu_prefix+i);
    if (this.isAncestor(e,elem)) {
    } else {
      this.hide_elm(e);
    }
  }
}

/* hides all menu items, but don't tough all DOM Nodes,
   that contents elem as descandant */
GLOBAL_MENU_72a2692.hide_all_but_list = 
  function hide_all_but_list (eleml) {

  for (var i = 0; i<=this.MENU_MAX_IDS; i++) {
    var e = this.menu_id_to_elm(i);
    if (e) {
      // set href if this is a not a leaf
//       if ((e.nodeName.toUpperCase() == "UL") && e.previousSibling ){
//         var a = e.previousSibling;
//         if ((a) && (a.getAttribute("onclick"))) {
//               a.href = a.getAttribute("onclick");
//               a.removeAttribute("onclick");
//         }
//       };
      // hide the element
      var j = 0, nIsAncestor = true;
      while (j < eleml.length && nIsAncestor ) {
        nIsAncestor = !(this.isAncestor(e,eleml[j]));
        j++;
      }
      if (nIsAncestor) {
        this.hide_elm(e);
      }
    }
  }
}

/* check if an element is visible. If it is, hide it, else show it */
GLOBAL_MENU_72a2692.toggle = function toggle(id) {
  var e = document.getElementById(id);
  if (e) {
    if ((e.style) && (e.style.display == "none"))
      this.show_elm(e);
    else
      this.hide_elm(e);
  }
}

/* hides all elements. Then show the on with the given ID.
   If onload is allready set, save the function in onload an
   starts it at beginning of s. */
// function ADD_ONLOAD_HANDLER(handler) {
//  var old_load = window.onload;
//  window.onload = function () {
//                    if (old_load) old_load();
//                    handler ();
//                  }
// }

/* hides all elments like hide_all_but, but it looks in 
   the cookies to find a list of elements that should be
   visible. */
Array.prototype.push = function (v) {
  var l = this.length;
  this[l] = v;
}
GLOBAL_MENU_72a2692.hide_all_but_with_cookies = 
function hide_all_but_with_cookies ( elm ) {
  var no_hides_str = this.cookie.getCookie( this.cookie.cookie_name );
  var no_hides = Array();
  if (no_hides_str.length > 0) {
    var no_hides_str_l = no_hides_str.split("|");
    for (x in no_hides_str_l) {
      if (no_hides_str_l[x].length > 0) {
        var e = this.menu_id_to_elm(no_hides_str_l[x]);
        no_hides.push(e);
        this.cookie.addId ( no_hides_str_l[x] );

      }
    }
  } else {
    for (x in this.shows) {
      var e = document.getElementById(this.shows[x]);
      no_hides.push(e);
    }
  }
  for (var x in no_hides) {
    this.cookie.addId(this.menu_elm_to_id( no_hides[x] ) );
  }
  if (elm) no_hides.push(elm);
  this.hide_all_but_list(no_hides);
}

GLOBAL_MENU_72a2692.setup = function setup(max, id, cookiename) {
  this.MENU_MAX_IDS = max;
  this.cookie.cookie_name = cookiename;
  var e = document.getElementById(id);
  if (e) {
    this.hide_all_but_with_cookies(e);
  } else {
    this.hide_all_but_with_cookies(null);
  }
}

GLOBAL_MENU_72a2692.shows = [];
GLOBAL_MENU_72a2692.add_visible = function(id) {
  this.shows.push(id); 
}
