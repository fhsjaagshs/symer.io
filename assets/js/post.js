var commentsDiv = document.getElementById("comments");
var postId = document.getElementsByClassName("post-title")[0].id;

function addDivNextTo(target, divelement) {
  divelement.style.marginLeft = (parseInt((target.style.marginLeft || "0")) + 50) + "px";
  target.parentNode.insertBefore(divelement, target.nextSibling);
}

function toArray(nl) {
    for (var a=[], l=nl.length; l--; a[l]=nl[l]);
    return a;
}

function screenScale() {
  var scale = 1;
  try {
    scale = window.devicePixelRatio;
  } catch(err) {
    scale = 1;
  }
  return scale;
}

function generateDOMObjects(html) {
  var div = document.createElement('div');
  div.innerHTML = html;
  for (var a=[], i=div.childNodes.length; i--; a[i]=div.childNodes[i]);
  return a;
}

function gravatarURL(email) { return "https://secure.gravatar.com/avatar/" + md5(email) + ".png?r=x&d=mm&s=" + 60*screenScale(); }

function editorHTML(h,position) {
  return '<div class="editor"' + (h == true ? " hidden" : "") + '>'
       + '<input type="text" class="comment-editor-text-field" id="emailfield" placeholder="Email">'
       + '<input type="text" class="comment-editor-text-field" id="namefield" placeholder="Display Name"' + (position == "center" ? 'style="margin: 0 auto;"' : "") +  '>'
       + '<textarea class="comment-editor-textarea" id="bodyfield" placeholder="Enter your comment here"' + (position == "center" ? 'style="margin: 0 auto;"' : "") +  '></textarea>'
       + '<a class="blogbutton" onclick="console.log("fuck yeah")">Comment</a>'
       + '</div>';
}

function commentHTML(email,name,body) {
  return '<img width="60px" height="60px" src="' + gravatarURL(email) + '" class="comment-avatar" onerror=\'this.src="https://secure.gravatar.com/avatar/?r=x&d=mm&s=" + 60*screenScale()\'>'
         + '<div class="comment-content">'
         + '<h3 class="comment-name">' + name + '</h3>'
         + '<p class="comment-body">' + body + '</p>'
         + '<a class="blogbutton" onclick="toggleEditor(this);">Reply</a>'
         + '</div>'
         + editorHTML(true,"left");
}

function toggleEditor(linkbtn) {
  linkbtn.parentNode.nextSibling.hidden = !linkbtn.parentNode.nextSibling.hidden;
  linkbtn.innerHTML = (linkbtn.innerHTML=='Reply'?'Cancel':'Reply');
}

function commentDiv(email,name,body,id) {
  var div = document.createElement("div");
  div.className = "comment";
  div.id = "comment" + id.toString();
  div.innerHTML = commentHTML(email,name,body);
  return div;
}

function comment(email, dispname, body, id) {
  commentsDiv.appendChild(commentDiv(email, dispname, body, id));
  // sendHTTP("POST", "/posts/" + postId + "comments", {}, function() {
  //
  // })
}

function reply(email, dispname, body, id, parentId) {
  if (parentId != -1) {
    addDivNextTo(
      document.getElementById("comment" + parentId.toString()),
      commentDiv(email, dispname, body, id)
    );
  }
}

function renderComments(comments, pid) {
  if (comments.length > 0) {
    var c = comments.shift();
    if (pid < 0) {
      comment(c.email, c.display_name, c.body, c.id);
    } else {
      reply(c.email, c.display_name, c.body, c.id, pid);
    }
    renderComments(c.children, c.id);
    renderComments(comments, pid);
  }
}

// create and add to the mix
window.onload = function() {
  commentsDiv.appendChild(generateDOMObjects(editorHTML(false,"center"))[0]);

  var sc = document.getElementById("spinner-container");
  var spinner = new Spinner({lines: 12, color: "#9A9A9A", hwaccel: true, top: '50%', left: '50%' }).spin(sc);
  sc.appendChild(spinner.el);

  sendHTTP("GET", "/posts/" + postId + "/comments.json", {}, function(http) {
    if (http.status == 200) {
      spinner.stop();
      commentsDiv.removeChild(document.getElementById("spinner-container"));
      renderComments(JSON.parse(http.responseText), -1);
    }
  });
}
