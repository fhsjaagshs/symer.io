
// TODO: if a post exists, load its contents into the editor

var vm = new Vue({
  el: '#editor',
  data: {
    input: ""
  },
  filters: {
    marked: marked
  }
})

var postId = -1
var editor = document.getElementById("editor")

if (editor != null) {
  var attrPostId = editor.getAttribute("post-id")
  if (attrPostId != null) {
    postId = parseInt(attrPostId)
  }
}

function postMarkdown() {
  var http = new XMLHttpRequest();
  var markdown = document.getElementById("markdown-textarea").value
  var title = ""
  // the order of this is important because of how JS strings work
  var params = "title=" + encodeURI(title)
  if (postId > -1) {
    params = params + "&identifier=" + encodeURI(postId.toString())
  }
  params = params + "&body=" + encodeURI(markdown)
  http.open("POST", "/posts", true);
  http.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
  http.setRequestHeader("Content-length", params.length);
  http.setRequestHeader("Connection", "close");
  http.onreadystatechange = function() {
    if (!(http.readyState == 4 && http.status == 200)) {
      alert(http.responseText)
    }
  }
  http.send(params);
}

document.getElementById("save-button").onclick = postMarkdown