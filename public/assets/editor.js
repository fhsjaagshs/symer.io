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

$.getScript("/assets/wordlist.js", function() {
  $(".wordlist").wordlist();
  $(".wordlist").on("wordlist:addedWord", function(e,word) { addTag(word); }); 
  $(".wordlist").on("wordlist:deletedWord", function(e,word) { deleteTag(word); });
});

var newTags = []
var deletedTags = []

function deleteTag(word) {
  deletedTags.push(word);
  newTags.splice(newTags.indexOf(word), 1);
}

function addTag(word) {
  newTags.push(word);
  deletedTags.splice(deletedTags.indexOf(word), 1);
}

saveButton.onclick = function() {
  var markdown = editor.value;
  var title = document.getElementById("title-field").value;
  
  if (title == undefined) { title = ""; }
  if (markdown == undefined) { markdown = ""; }
  
  var params = "title=" + encodeURI(title);
  if (postId != -1) { params += "&id=" + encodeURI(postId.toString()); }
  if (newTags.length > 0) { params += "&tags=" + ((postId == -1) ? encodeURI(newTags.filter(function(e) {return !deletedTags.indexOf(e) > -1;}).join()) : encodeURI(newTags.join())); }
  if (deletedTags.length > 0) { params += "&deleted_tags=" + encodeURI(deletedTags.join()); }
  params += "&body=" + encodeURI(markdown); // appended last because of how JS strings work
  
  console.log(params)

  sendHTTP("POST", "/posts", params, function(http) {
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