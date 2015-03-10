var editing = true;
var editor = document.getElementById("editor");
var preview = document.getElementById("preview");
var previewButton = document.getElementById("preview-button");
var deleteButton = document.getElementById("delete-button");
var saveButton = document.getElementById("save-button");
var titleField = document.getElementById("title-field");

$.getScript("/assets/wordlist.js", function() {
  $(".wordlist").wordlist();
  $(".wordlist").on("wordlist:addedWord", function(e,word) { addTag(word); }); 
  $(".wordlist").on("wordlist:deletedWord", function(e,word) { deleteTag(word); });
});

var post = {}
post.tags = []
post.deleted_tags = []
var attrPostId = editor.getAttribute("post-id");
if (attrPostId) { post.id = parseInt(attrPostId); }

function deleteTag(word) {
  post.deleted_tags.push(word);
  var idx = post.tags.indexOf(word)
  if (idx != -1) post.tags.splice(idx, 1);
}

function addTag(word) {
  post.tags.push(word);
  var idx = post.deleted_tags.indexOf(word)
  if (idx != -1) post.deleted_tags.splice(idx, 1);
}

editor.oninput = function() {
  post.body = editor.value;
}

titleField.oninput = function() {
  post.title = titleField.value;
}

saveButton.onclick = function() {
  if (post.tags.length > 0) post.tags = post.tags.join();
  else delete post.tags;
  if (post.deleted_tags.length > 0) {
    if (post.id == -1) delete post.deleted_tags;
    else post.deleted_tags = post.deleted_tags.join();
  } else delete post.deleted_tags;

  sendHTTP("POST", "/posts", post, function(http) {
    if (http.readyState == 4 && http.status == 200) {
      window.location.href = http.getResponseHeader("Location");
    }
  })
}

previewButton.onclick = function() {
  editing = !editing;
  previewButton.innerHTML = editing ? "Preview" : "Edit" ;
  editor.style.display = editing ? "block" : "none";
  preview.innerHTML = editing ? "" : "<div class=\"post-content\">" + marked(editor.value) + "</div>";
}

deleteButton.onclick = function() {
  if (confirm("Are you sure you want to delete this post?")) {
    sendHTTP("DELETE", "/posts/" + postId, {}, function(http) {
      if (http.readyState == 4 && http.status == 200) {
        window.location.href = "/"
      }
    });
  }
}

// callback: function(http) { ... }
function sendHTTP(method, url, params, callback) {  
  var http = new XMLHttpRequest();
  http.open(method, url, true);
  http.onreadystatechange = function() { callback(http); }
  
  if (Object.size(params) > 0) {
    var body = Object.keys(params).map(function(key, _) { return key + "=" + encodeURI(params[key]); }).join("&");
    http.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    http.send(body);
  } else {
    http.send();
  }
}

Object.size = function(obj) {
    var size = 0, key;
    for (key in obj) {
        if (obj.hasOwnProperty(key)) size++;
    }
    return size;
};