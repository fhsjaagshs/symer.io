var commentsDiv = document.getElementById("comments");

function addDivNextTo(target, divelement) {
  divelement.style.marginLeft = (parseInt((target.style.marginLeft || "0")) + 50) + "px";
  target.parentNode.insertBefore(divelement, target.nextSibling);
}

function gravatarURL(email) { return "https://secure.gravatar.com/avatar/" + md5(email) + ".png?s=180&r=x&d=mm"; }

function commentHTML(email,name,body) {
  return '\<img src="' + gravatarURL(email) + '">'
        + '\<div>'
        + '\<h3>' + name + "\</h3>"
        + "\<p>" + body + "\</p>"
        + "\<a onclick='reply(getEmail(), getName(), getBody(), this.parentNode.id)'>Reply\</a></div>";
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
    console.log(c);
  }
}

console.log("asdf");

sendHTTP("GET", "/posts/" + document.getElementsByClassName("post-title")[0].id + "/comments.json", {}, function(http) {
  if (http.status == 200) {
    console.log(JSON.parse(http.responseText));
    renderComments(JSON.parse(http.responseText), -1);
  }
});

// TODO: Load posts from JSON