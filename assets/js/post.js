var commentsDiv = document.getElementById('comments');

function addDivNextTo(target, divelement) {
  divelement.style.marginLeft = (parseInt((target.style.marginLeft || "0")) + 50) + "px";
  console.log(divelement.style.marginLeft);
  target.parentNode.insertBefore(divelement, target.nextSibling);
}

function gravatarURL(email) { return "https://secure.gravatar.com/avatar/" + md5(email) + ".png?s=180&r=x&d=mm"; }

function commentHTML(email,name,body) {
  return '\<img src="' + gravatarURL(email) + '" width=90 height=90>'
        + '\<h3>' + name + "\</h3>"
        + "\<p>" + body + "\</p>"
        + "\<a onclick='reply(getEmail(), getName(), getBody(), this.parentNode.id)'>Reply\</a>";
}

function commentDiv(email,name,body) {
  var div = document.createElement("div");
  div.style.width = "500px";
  div.style.margin = "10px";
  div.style.padding = "10px";
  div.className = "comment";
  div.id = Math.floor((Math.random()*100000000) + 1).toString();
  div.style.backgroundColor = "green";
  div.innerHTML = commentHTML(email,name,body);
  return div;
}

function comment(email, dispname, body) {
  var div = commentDiv(email, dispname, body);
  commentsDiv.body.appendChild(div);
}

function reply(email, dispname, body, parentId) {
  addDivNextTo(
    document.getElementById(parentId),
    commentDiv(email, dispname, body)
  );
}