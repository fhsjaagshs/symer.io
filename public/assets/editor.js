for (var stylesheet in ["/assets/css/editor.css","/assets/css/wordlist.css"]) {
  var l = document.createElement("link");
  l.rel = "stylesheet";
  l.href = stylesheet;
  document.head.appendChild(l);
}

var postId = -1;
var editing = true;
var editor = document.getElementById("editor");
var preview = document.getElementById("preview");
var previewButton = document.getElementById("preview-button");
var deleteButton = document.getElementById("delete-button");
var saveButton = document.getElementById("save-button");

previewButton.style.marginLeft = document.getElementById("content").offsetWidth-deleteButton.offsetWidth;

var attrPostId = editor.getAttribute("post-id");
if (attrPostId != null) { postId = parseInt(attrPostId); }

$.getScript( "/assets/wordlist.js", function() {
  $(".wordlist").wordlist();
  $(".wordlist").placeholder = "Enter tag"
  $(".wordlist").on("wordlist:addedWord", function(e, word) { addTag(word); }); 
  $(".wordlist").on("wordlist:deletedWord", function(e,word) { deleteTag(word); });
});

var accumulated = []; // accumulate added/deleted tags if the post isn't created yet.

function deleteTag(word) {
  if (postId == -1) { accumulated.splice(accumulated.indexOf(word), 1); }
  else { sendHTTP("DELETE", "/posts/"+postId+"/tag", "tag=" + encodeURI(word), function(http) {}); }
}

function addTag(word) {
  if (postId == -1) { accumulated.push(word); }
  else { sendHTTP("POST", "/posts/"+postId+"/tag", "tag=" + encodeURI(word), function(http) {}); }
}

saveButton.onclick = function() {
  var markdown = editor.value;
  var title = document.getElementById("title-field").value;
  
  if (title == undefined) { title = ""; }
  if (markdown == undefined) { markdown = ""; }
  
  // the order of this is important because of how JS strings work
  var params = "title=" + encodeURI(title);
  if (postId != -1) { params = params + "&id=" + encodeURI(postId.toString()); }
  else { params = params + "&tags=" + encodeURI(accumulated.join()); }
  params = params + "&body=" + encodeURI(markdown);

  sendHTTP("POST", "/posts", params, function(http) {
    if (!(http.readyState == 4 && http.status == 200)) {
      postId = JSON.parse(http.responseText).id;
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
    sendHTTP("DELETE", "/posts/" + postId, "", function(http) {
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
  
  if (params.length > 0) {
    http.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    http.send(params)
  } else {
    http.send();
  }
}