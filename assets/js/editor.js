// TODO:
// 1. Improve preview code
// 2. Double check tags posting code
// 3. fix lexical error in marked
function Post(id) {
  this.tags = [];
  this.id = id;
  this.draft = true;
}

Post.prototype = {
  deleteTag: function(tag) {
    var idx = this.tags.indexOf(tag);
    if (idx != -1) this.tags.splice(idx, 1);
  },
  addTag: function(tag) {
    this.tags.push(tag);
  }
}

var editor = document.getElementById("editor");
var preview = document.getElementById("preview");
var previewButton = document.getElementById("preview-button");
var deleteButton = document.getElementById("delete-button");
var saveButton = document.getElementById("save-button");
var titleField = document.getElementById("title-field");

// variable @tags@ is defined prior to this script's evaluation

var post = new Post(parseInt(editor.getAttribute("post-id")));
post.tags = tags;

var wordList = new WordList(tags);
wordList.onAdd = function(word) { post.addTag(word); }
wordList.onDelete = function(word) { post.deleteTag(word); }

editor.parentNode.insertBefore(wordList.div, editor.nextSibling);

document.getElementById("public-checkbox").onchange = function(e) {
  post.draft = e.target.checked ? "False" : "True";
}

editor.oninput = function() {
  post.body = editor.value;
}

titleField.oninput = function() {
  post.title = titleField.value;
}

saveButton.onclick = function() {
  post.tags = post.tags.join();
  if (post.id == -1) delete post.id; 
  console.log(post);
  sendHTTP("POST", "/posts", post, function(http) {
    post = new Post();
    if (http.status == 200)
      window.location.href = http.getResponseHeader("Location");
  });
}

var editing = true;
previewButton.onclick = function() {
  editing = !editing;
  previewButton.innerHTML = editing ? "Preview" : "Edit";
  editor.style.display = editing ? "block" : "none";
  preview.innerHTML = editing ? "" : "<div class=\"post-content\">" + marked(post.body) + "</div>";
}

deleteButton.onclick = function() {
  if (confirm("Are you sure you want to delete this post?"))
    sendHTTP("DELETE", "/posts/" + post.id, {}, function(http) {
      if (http.status == 200)
        window.location.href = "/";
    });
}