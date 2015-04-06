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
var publicCheckbox = document.getElementById("public-checkbox");


function Post() {
  this.tags = [];
  this.deleted_tags = [];
  this.id = parseInt(editor.getAttribute("post-id"));
}

var post = new Post();

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
  console.log(post);
  if (post.tags.length > 0) {
    post.tags = post.tags.join();
  } else {
    delete post.tags;
  }
  
  if (post.deleted_tags.length > 0) {
    if (post.id == -1) {
      delete post.deleted_tags
    } else {
      post.deleted_tags = post.deleted_tags.join();
    }
  } else {
    delete post.deleted_tags;
  }
  
  if (post.id == -1) delete post.id; 
  
  post.draft = publicCheckbox.checked ? "False" : "True";
  
  console.log("SHIT");
  
  sendHTTP("POST", "/posts", post, function(http) {
    post = new Post();
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