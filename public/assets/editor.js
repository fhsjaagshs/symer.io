var editorcss = document.createElement("link");
editorcss.rel = "stylesheet";
editorcss.href = "/assets/editor.css";
document.head.appendChild(editorcss);

var wordlist = document.createElement("script");
wordlist.src = "/assets/wordlist.min.js";
document.head.appendChild(wordlist);

var postId = -1;
var editing = true;
var editor = document.getElementById("editor");
var preview = document.getElementById("preview");
var previewButton = document.getElementById("preview-button");
var deleteButton = document.getElementById("delete-button");
var saveButton = document.getElementById("save-button");

var attrPostId = editor.getAttribute("post-id");
if (attrPostId != null) { postId = parseInt(attrPostId); }

$.getScript( "/assets/wordlist.min.js", function() {
  $(".wordlist").wordlist();
  
  $(".wordlist").on("wordlist:addedWord", function(e, word) {
    var params = "tag=" + encodeURI(word);
    http.open("POST", "/posts/" + postId + "/tag", true);
    http.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    http.setRequestHeader("Content-length", params.length);
    http.setRequestHeader("Connection", "close");
    http.send(params);
  })
  
  $(".wordlist").on("wordlist:deletedWord", function(e,word)) {
    var params = "tag=" + encodeURI(word);
    http.open("DELETE", "/posts/" + postId + "/tag", true);
    http.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    http.setRequestHeader("Content-length", params.length);
    http.setRequestHeader("Connection", "close");
    http.send(params);
  }
});

saveButton.onclick = function() {
  var http = new XMLHttpRequest();
  var markdown = editor.value;
  var title = document.getElementById("title-field").value;
  
  if (title == undefined) { title = ""; }
  if (markdown == undefined) { markdown = ""; }
  
  // the order of this is important because of how JS strings work
  var params = "title=" + encodeURI(title);
  if (postId > -1) { params = params + "&id=" + encodeURI(postId.toString()); }
  params = params + "&body=" + encodeURI(markdown);
  http.open("POST", "/posts", true);
  http.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
  http.setRequestHeader("Content-length", params.length);
  http.setRequestHeader("Connection", "close");
  http.onreadystatechange = function() {
    if (!(http.readyState == 4 && http.status == 200)) {
      window.location.href = http.getResponseHeader("Location");
    }
  }
  http.send(params);
}

previewButton.onclick = function() {
  editing = !editing;
  previewButton.innerHTML = editing ? "Preview" : "Edit" ;
  editor.style.display = editing ? "block" : "none";
  preview.innerHTML = editing ? "" : "<div class=\"post-content\">" + marked(editor.value) + "</div>";
}

deleteButton.onclick = function() {
  http.open("POST", "/posts/"+postId, true);
  http.setRequestHeader("Connection", "close");
  http.onreadystatechange = function() {
    if (!(http.readyState == 4 && http.status == 200)) {
      window.location.href = "/"
    }
  }
  http.send();
}