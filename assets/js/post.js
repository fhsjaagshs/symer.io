"use strict";

var CommentField = function CommentField(aPlaceholder) {
  this.el = document.createElement('input');
  this.el.type = 'text';
  this.el.className = 'comment-field';
  this.el.placeholder = aPlaceholder;
  this.el.__ref = this;
}

var CommentTextarea = function CommentTextarea(aPlaceholder) {
  this.el = document.createElement('textarea');
  this.el.className = 'comment-textarea';
  this.el.placeholder = aPlaceholder;
  this.el.__ref = this;
}

var Editor = function Editor(hidden, id) {
  this.nameField = new CommentField('Name');
  this.emailField = new CommentField('Email');
  this.textarea = new CommentTextarea('Start writing...');
  
  this.id = id;
  
  var goButton = document.createElement('div');
  goButton.className = 'gobutton';
  goButton.editor = this;
  goButton.onclick = function() {
    var e = this.editor;
    
    var params = {
      email: e.email,
      display_name: e.name,
      body: e.commentBody
    };
    
    if (e.id) {
      params.parent_id = e.id;
      e.hide();
    }

    e.reset();
    console.log(e.id);
    sendHTTP('POST', '/posts/' + postId + '/comments', params, function(http) {
      if (http.status == 200) {
        console.log(e.id);
        console.log('-');
        params.id = +http.responseText;
        var c = Comment.fromJSON(params);
        if (e.id == -1) {
          comment(c);
        } else {
          reply(c, e.id); // FIXME: Adds to last comment
        }
      } else {
        alert('Failed to post comment.');
      }
    });
  }
  
  var topFields = document.createElement('div');
  topFields.className = 'top-fields';
  topFields.appendChild(this.nameField.el);
  topFields.appendChild(this.emailField.el);
  
  var bottomFields = document.createElement('div');
  topFields.className = 'bottom-fields';
  bottomFields.appendChild(this.textarea.el);
  bottomFields.appendChild(goButton);

  this.el = document.createElement('div');
  this.el.className = 'editor';
  this.el.style.display = hidden?'none':'block';
  this.el.appendChild(topFields);
  this.el.appendChild(bottomFields);
  this.el.__ref = this;
}

Editor.prototype = {
  isVisible: function() {
    return this.el.style.display != 'none';
  },
  hide: function() {
    this.el.style.display = 'none';
  },
  show: function() {
    this.el.style.display = 'block';
  },
  reset: function() {
    this.commentBody = '';
    this.email = '';
    this.name = '';
  }
}

Object.defineProperty(Editor.prototype, 'commentBody', {
    get: function() { return this.textarea.el.value },
    set: function(v) { this.textarea.el.value = v; }
});

Object.defineProperty(Editor.prototype, 'email', {
    get: function() { return this.emailField.el.value; },
    set: function(v) { this.emailField.el.value = v; }
});

Object.defineProperty(Editor.prototype, 'name', {
    get: function() { return this.nameField.el.value; },
    set: function(v) { this.nameField.el.value = v; }
});

var Comment = function Comment(email,dname,body,id) {
  var editor = new Editor(true, id);
  
  this.id = id;

  this.el = document.createElement('div');
  this.el.className = 'comment';
  this.el.id = 'comment' + id;
  this.el.__ref = this;
  this.editor = editor;
  
  this.commentName = document.createElement('h3');
  this.commentName.className = 'comment-name';
  this.commentName.innerHTML = dname;
  
  this.commentBody = document.createElement('p');
  this.commentBody.className = 'comment-body';
  this.commentBody.innerHTML = body;
  
  this.button = document.createElement('a');
  this.button.className = 'blogbutton';
  this.button.innerHTML = 'Reply';
  this.button.onclick = function() {
    if (editor.isVisible()) {
      this.innerHTML = 'Reply';
      editor.hide();
    } else {
      this.innerHTML = 'Cancel';
      editor.show();
    }
  }
  
  this.children = [];
  this.parent = null;
  
  this.el.appendChild(this.commentName);
  this.el.appendChild(this.commentBody);
  this.el.appendChild(this.button);
  this.el.appendChild(this.editor.el);
}

Comment.prototype = {
  appendChild: function(child) {
    if (child.parent == null && this.children.indexOf(child) == -1) {
      var indent = (parseInt(this.el.style.marginLeft) || 0) + 50;
      child.el.style.marginLeft = indent + "px";
      this.el.parentNode.insertBefore(child.el, this.el.nextSibling);
      this.children.push(child);
      child.parent = this;
    }
  }
}

Comment.fromJSON = function(json) {
  return new Comment(json.email, json.display_name, json.body, json.id);
}

function comment(c) {
  commentsDiv.appendChild(c.el);
}

function reply(c, parentId) {
  if (parentId != -1) {
    var parentElem = document.getElementById("comment" + parentId);
    if (parentElem) parentElem.__ref.appendChild(c);
  }
}

function renderComments(comments, pid) {
  if (comments.length > 0) {
    var c = comments.shift();
    if (pid == -1) comment(Comment.fromJSON(c));
    else           reply(Comment.fromJSON(c), pid);
    renderComments(c.children, c.id);
    renderComments(comments, pid);
  }
}

// Globals
var commentsDiv = document.getElementById('comments');
var postId = +document.getElementsByClassName('post-title')[0].id;
var rootEditor = commentsDiv.appendChild((new Editor(false, -1)).el).__ref;

window.onload = function() {
  sendHTTP('GET', '/posts/' + postId + '/comments.json', {}, function(http) {
    if (http.status == 200) {
      renderComments(JSON.parse(http.responseText), -1);
    }
  });
}
