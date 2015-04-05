$(".wordlist").wordlist();
$(".wordlist").on("wordlist:addedWord", function(e,word) { addTag(word); }); 
$(".wordlist").on("wordlist:deletedWord", function(e,word) { deleteTag(word); });

var editing = true;
var editor = document.getElementById("editor");
var preview = document.getElementById("preview");
var previewButton = document.getElementById("preview-button");
var deleteButton = document.getElementById("delete-button");
var saveButton = document.getElementById("save-button");
var titleField = document.getElementById("title-field");

var post = {};
post.tags = [];
post.deleted_tags = [];
var attrPostId = editor.getAttribute("post-id");
if (attrPostId) post.id = parseInt(attrPostId);

function deleteTag(word) {
  post.deleted_tags.push(word);
  var idx = post.tags.indexOf(word);
  if (idx != -1) post.tags.splice(idx, 1);
}

function addTag(word) {
  post.tags.push(word);
  post.deleted_tags.delete(word);
}

editor.oninput = function() {
  post.body = editor.value;
}

titleField.oninput = function() {
  post.title = titleField.value;
}

saveButton.onclick = function() {
  if (post.tags.length > 0) 
    post.tags = post.tags.join();
  else
    post.delete('tags');
  
  if (post.deleted_tags.length > 0) {
    if (post.id == -1) 
      post.delete('deleted_tags');
    else 
      post.deleted_tags = post.deleted_tags.join();
  } else
    post.delete('deleted_tags');
  
  post.draft = $("#public-checkbox").is(":checked") ? "False" : "True";

  sendHTTP("POST", "/posts", post, function(http) {
    if (http.status == 200)
      window.location.href = http.getResponseHeader("Location");
  });
}

previewButton.onclick = function() {
  editing = !editing;
  previewButton.innerHTML = editing ? "Preview" : "Edit";
  editor.style.display = editing ? "block" : "none";
  preview.innerHTML = editing ? "" : "<div class=\"post-content\">" + marked(editor.value) + "</div>";
}

deleteButton.onclick = function() {
  if (confirm("Are you sure you want to delete this post?"))
    sendHTTP("DELETE", "/posts/" + post.id, {}, function(http) {
      if (http.status == 200)
        window.location.href = "/";
    });
}

/*
  method (String) The HTTP method to use
  url (String) duh
  params (Object) An object containing the parameters (eg {myparam: "myvalue"})
  callback (Function) A function that will be called with the XMLHTTPRequest object (eg function(http) { ... })
*/
function sendHTTP(method, url, params, callback) {  
  sendHTTPRaw(
    method,
    url,
    params.map(function (k,v) { return k + "=" + encodeURIComponent(v); }),
    "application/x-www-form-urlencoded",
    callback
  );
}

function sendHTTPRaw(method, url, body, contentType, callback) {
  var http = new XMLHttpRequest();
  http.open(method, url, true);
  
  http.onreadystatechange = function() { 
    if (method == "HEAD" && http.readyState == 2)
      if (callback) callback(http);
    else if (http.readyState == 4)
      if (callback) callback(http);
  }
    
  if (body.length > 0) {
    http.setRequestHeader("Content-type", contentType);
    http.send(body);
  } else
    http.send();
}

Array.prototype.deleteAt = function(idx) { this.splice(idx, 1); }
Array.prototype.delete = function(v) {
  var idx = this.indexOf(v);
  if (idx != -1) this.deleteAt(idx);
}

Object.prototype.delete = function(k) { delete this[k]; }
Object.prototype.size = function() { return this.keys().length; };
Object.prototype.map = function(f) {
  var o = this;
  return o.keys().map(function (k) { return f(k,o[k]); });
}
Object.prototype.keys = function() {
  var obj = this;
  return Object.keys(obj).filter(function(v) { return obj.hasOwnProperty(v); });
};
