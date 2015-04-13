var commentsDiv = document.getElementById("comments");

function addDivNextTo(target, divelement) {
  divelement.style.marginLeft = (parseInt((target.style.marginLeft || "0")) + 50) + "px";
  target.parentNode.insertBefore(divelement, target.nextSibling);
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

function gravatarURL(email) { return "https://secure.gravatar.com/avatar/" + md5(email) + ".png?s=" + 60*screenScale() + "&r=x&d=mm"; }

function editorHTML(h) {
  return '<div class="editor"' + (h == true ? " hidden" : "") + '>'
       + '<input type="text" class="commentfield blogtextfield" id="emailfield" name="email" placeholder="Email">'
       + '<input type="text" class="blogtextfield commentfield" id="namefield" name="display_name" placeholder="Display Name">'
       + '<input type="text" class="blogtextfield commentfield" id="bodyfield" name="body" placeholder="Enter your comment here">'
       + '<a class="blogbutton commentbutton" onclick="console.log("fuck yeah")">Comment</a>'
       + '</div>';
}

function commentHTML(email,name,body) {
  return '<img width="60px" height="60px" src="' + gravatarURL(email) + '" class="comment-avatar">'
         + '<div class="comment-content">'
         + '<h3 class="comment-name">' + name + '</h3>'
         + '<p class="comment-body">' + body + '</p>'
         + '<a class="blogbutton replybutton" onclick="toggleEditor(this);">Reply</a>'
         + '</div>'
         + editorHTML(true);
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
  var div = commentDiv(email, dispname, body, id);
  commentsDiv.appendChild(div);
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
  sendHTTP("GET", "/posts/" + document.getElementsByClassName("post-title")[0].id + "/comments.json", {}, function(http) {
    if (http.status == 200) {
      [].slice.call(document.getElementsByClassName("spinner")).map(function (x) { commentsDiv.removeChild(x); });
      renderComments(JSON.parse(http.responseText), -1);
    }
  });
}
