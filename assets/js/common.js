/*
  method (String) The HTTP method to use
  url (String) duh
  params (Object) An object containing the parameters (eg {myparam: "myvalue"})
  callback (Function) A function that will be called with the XMLHTTPRequest object (eg function(http) { ... })
*/
function sendHTTP(method, url, params, callback) {  
  return sendHTTPRaw(
    method,
    url,
    Object.map(params, function (k,v) { return k + "=" + encodeURIComponent(v); }).join("&"),
    "application/x-www-form-urlencoded",
    callback
  );
}

function sendHTTPRaw(method, url, body, contentType, callback) {
  var http = new XMLHttpRequest();
  http.open(method, url, true);
  http.setRequestHeader("Content-type", contentType);
  http.onreadystatechange = function() { 
    if (method == "HEAD" && http.readyState == 2) {
      if (callback) callback(http);
    } else if (http.readyState == 4) {
      if (callback) callback(http);
    }
  }
  
  return http.send(body);
}

Array.prototype.deleteAt = function(idx) { this.splice(idx, 1); }
Array.prototype.delete = function(v) {
  var idx = this.indexOf(v);
  if (idx != -1) this.deleteAt(idx);
}
Object.size = function(o) { return o.keys().length; };
Object.map = function(o, f) { return Object.keys(o).filter(function(v) { return o.hasOwnProperty(v); }).map(function (k) { return f(k,o[k]); }); }
