var ee = new EpicEditor({
  container: 'editor',
  textarea: 'markdown-textarea',
  basePath: '',
  clientSideStorage: false,
  useNativeFullscreen: true,
  parser: marked,
  theme: {
    base: '/assets/epiceditor/base.css',
    preview: '/assets/epiceditor/preview.css',
    editor: '/assets/epiceditor/editor.css'
  },
  button: {
    preview: true,
    fullscreen: false,
    bar: "auto"
  },
  focusOnLoad: true,
  shortcut: {
    modifier: 18,
    fullscreen: 70,
    preview: 80
  },
  string: {
    togglePreview: 'Show Preview',
    toggleEdit: 'Back to Editing'
  },
  autogrow: false
}).load();

window.onresize = function () { ee.reflow(); }
window.onload = function () { ee._setupTextareaSync(); }

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
  var title = document.getElementById("title-field").value
  
  if (title == undefined) title = ""
  if (markdown == undefined) markdown = ""
  
  // the order of this is important because of how JS strings work
  var params = "title=" + encodeURI(title)
  if (postId > -1) {
    params = params + "&id=" + encodeURI(postId.toString())
  }
  params = params + "&body=" + encodeURI(markdown)
  http.open("POST", "/posts", true);
  http.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
  http.setRequestHeader("Content-length", params.length);
  http.setRequestHeader("Connection", "close");
  http.onreadystatechange = function() {
    if (!(http.readyState == 4 && http.status == 200)) {
      window.location.replace(http.getResponseHeader("Location"));
    }
  }
  http.send(params);
}

document.getElementById("save-button").onclick = postMarkdown