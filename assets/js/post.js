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
  for (var a = [], i = div.childNodes.length; i--; a[i] = div.childNodes[i]);
  return a;
}

function gravatarURL(email) { return "https://secure.gravatar.com/avatar/" + md5(email) + ".png?r=x&d=mm&s=" + 60*screenScale(); }

function editorHTML(h,id) {
  return '<div class="editor"' + (h == true ? ' style="display: none;"' : '') + '>'
       + '<input type="text" class="comment-editor-text-field" placeholder="Email">'
       + '<input type="text" class="comment-editor-text-field" placeholder="Display Name">'
       + '<textarea class="comment-editor-textarea" placeholder="Enter your comment here"></textarea>'
       + '<a class="blogbutton" onclick="comment_bang(this, ' + id + ')">Comment</a>'
       + '</div>';
}

function commentHTML(email,name,body,id) {
  return '<img width="60px" height="60px" src="' + gravatarURL(email) + '" class="comment-avatar" onerror="this.src="https://secure.gravatar.com/avatar/?r=x&d=mm&s=" + 60*screenScale()">'
         + '<div class="comment-content">'
         + '<h3 class="comment-name">' + name + '</h3>'
         + '<p class="comment-body">' + body + '</p>'
         + '<a class="blogbutton" onclick="this.innerHTML = (this.innerHTML == \'Reply\'?\'Cancel\':\'Reply\');toggleEditor(this.parentNode.nextSibling);">Reply</a>'
         + '</div>'
         + editorHTML(true,id);
}

function toggleEditor(editor) {
  if (editor.style.display == "none") {
    editor.childNodes[0].value = "";
    editor.childNodes[1].value = "";
    editor.childNodes[2].value = "";
  }
  
  editor.style.display = (editor.style.display=="none"?"block":"none");
}

function commentDiv(email,name,body,id) {
  var div = document.createElement("div");
  div.className = "comment";
  div.id = "comment" + id.toString();
  div.innerHTML = commentHTML(email,name,body,id);
  return div;
}

function comment_bang(commentButton, parentId) {
  var e = commentButton.parentNode;
  
  var params = {
    post_id:postId,
    email:e.childNodes[0].value,
    display_name:e.childNodes[1].value,
    body:e.childNodes[2].value
  };
  
  e.childNodes[0].value = "";
  e.childNodes[1].value = "";
  e.childNodes[2].value = "";
  
  if (parentId) {
    params["parent_id"] = parentId;
    e.style.display = (e.style.display=="none"?"block":"none");
  } else {
    e.parentNode.open = false;
  }
  
  sendHTTP("POST", "/posts/" + postId + "/comments", params, function(http) {
    if (http.status == 200) {
      if (parentId) reply(params['email'], params['display_name'], params['body'], parseInt(http.responseText), parentId);
      else          comment(params['email'], params['display_name'], params['body'], parseInt(http.responseText));
    } else {
      alert("Failed to post comment.");
    }
  });
}

function comment(email, dispname, body, id) {
  commentsDiv.appendChild(commentDiv(email, dispname, body, id));
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

window.onload = function() {
  commentsDiv.appendChild(generateDOMObjects('<details><summary class="unselectable" style="outline:none;cursor:pointer">Write comment</summary>' + editorHTML(false,null) + '</details>')[0]);

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
