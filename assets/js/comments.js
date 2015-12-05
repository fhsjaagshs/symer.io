"use strict";

var postId = +document.getElementsByClassName('post-title')[0].id;

//////////////////////////////////////////////////////////////////////
var CommentField = function CommentField(aPlaceholder) {
  var el = document.createElement('input');
  el.type = 'text';
  el.className = 'comment-field';
  el.placeholder = aPlaceholder;
  return el;
}

var CommentTextarea = function CommentTextarea(aPlaceholder) {
  var el = document.createElement('textarea');
  el.className = 'comment-textarea';
  el.placeholder = aPlaceholder;
  return el;
}

// TODO: make this a form?
var Editor = function Editor(hidden, comment, callback) {
  this.comment = comment;
  
  this.nameField = new CommentField('Name');
  this.emailField = new CommentField('Email (not displayed)');
  this.textarea = new CommentTextarea('Speak your mind...');
  
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
    
    if (e.comment) {
      params.parent_id = e.comment.id;
      e.hide();
    }
    
    e.reset();
    
    sendHTTP('POST', '/posts/' + postId + '/comments', params, function(http) {
      if (http.status == 200) {
        var c = new Comment(JSON.parse(http.responseText), null);
        if (e.comment) e.comment.appendChild(c);
        else Comment.div.appendChild(c.containerDiv);
        callback(true);
      } else {
        callback(false);
      }
    });
  }
  
  var topFields = document.createElement('div');
  topFields.className = 'top-fields';
  topFields.appendChild(this.nameField);
  topFields.appendChild(this.emailField);
  
  var bottomFields = document.createElement('div');
  topFields.className = 'bottom-fields';
  bottomFields.appendChild(this.textarea);
  bottomFields.appendChild(goButton);

  this.el = document.createElement('div');
  this.el.className = 'editor';
  this.el.style.display = hidden?'none':'block';
  this.el.style.textAlign = "center"
  this.el.appendChild(topFields);
  this.el.appendChild(bottomFields);
  this.el.__ref = this;
}

Editor.prototype = {
  isVisible: function() { return this.el.style.display != 'none'; },
  hide: function() { this.el.style.display = 'none'; },
  show: function() { this.el.style.display = 'block'; },
  reset: function() {
    this.commentBody = '';
    this.email = '';
    this.name = '';
  }
}

Object.defineProperty(Editor.prototype, 'commentBody', {
    get: function() { return this.textarea.value },
    set: function(v) { this.textarea.value = v; }
});

Object.defineProperty(Editor.prototype, 'email', {
    get: function() { return this.emailField.value; },
    set: function(v) { this.emailField.value = v; }
});

Object.defineProperty(Editor.prototype, 'name', {
    get: function() { return this.nameField.value; },
    set: function(v) { this.nameField.value = v; }
});

/*
  comment structure

  <div class="comment-container">
    <div class="comment">
      // content, editor, etc
    </div>
    <div class="comment">
      // content, editor, etc
    </div>
    // etc
  </div>

  "root" comments hold a reference to a @comment-container@
  and "child" comments hold a reference to their "root
  ancestor"'s @comment-container@.
*/

var Comment = function Comment(json, prnt) {
  this.commentDiv = document.createElement('div');
  this.commentDiv.className = 'comment';
  this.commentDiv.__ref = this;
  
  this.commentName = document.createElement('h3');
  this.commentName.className = 'comment-name';
  
  this.commentBody = document.createElement('p');
  this.commentBody.className = 'comment-body';
  
  var button = document.createElement('a');
  this.button = button;
  this.button.className = 'blogbutton';
  this.button.innerHTML = 'Reply';
  this.button.editor = new Editor(true, this, function(succeeded) {
    if (!succeeded) alert('failed to post comment.');
    button.innerHTML = 'Reply';
  });
  this.button.onclick = function() {
    if (this.editor.isVisible()) {
      this.innerHTML = 'Reply';
      this.editor.hide();
    } else {
      this.innerHTML = 'Cancel';
      this.editor.show();
    }
  }

  this.commentDiv.appendChild(this.commentName);
  this.commentDiv.appendChild(this.commentBody);
  this.commentDiv.appendChild(this.button);
  this.commentDiv.appendChild(this.button.editor.el);
  
  this.id = json.id;
  this.children = [];
  this.parent = prnt; // see setter, appends this.commentDiv to this.containerDiv
  this.displayName = json.display_name;
  this.email = json.email;
  this.body = json.body;
  this.timestamp = json.timestamp;
  
  var color = '#'+Math.floor(Math.random()*16777215).toString(16);
  for (var i = 0; i < json.children.length; i++) {
    new Comment(json.children[i],this); // comment contstructor adds constructed comment to parent's DOM tree
  }
}

Object.prototype.insertAfter = function (newNode) { this.parentNode.insertBefore(newNode, this.nextSibling); }

Comment.prototype = {
  appendChild: function(child) { child.parent = this }
}

Object.defineProperty(Comment.prototype, 'displayName', {
  get: function() { return this.commentName.innerHTML },
  set: function(v) { this.commentName.innerHTML = v; }
});

Object.defineProperty(Comment.prototype, 'body', {
  get: function() { return this.commentBody.innerHTML },
  set: function(v) { this.commentBody.innerHTML = v; }
});

Object.defineProperty(Comment.prototype, 'parent', {
  enumerable: true,
  writeable: true,
  get: function() { return this.__parent || null },
  set: function(prnt) {
    // ensure idempotence
    if (this.parent && this.parent == prnt) return;
    
    // remove @this@ from existing parent's children array
    if (this.parent) {
      var idx = this.parent.children.indexOf(this);
      if (idx > -1) this.parent.children.splice(idx, 1);
    }
    
    // remove @this.commentDiv@ from its container-div if it has one
    if (this.containerDiv && this.commentDiv.parentNode == this.containerDiv) {
      this.containerDiv.removeChild(this.commentDiv);
    }
    
    // set property
    this.__parent = prnt;
    
    if (this.parent) {
      // add @this@ to the children array and
      // ensure it's at the end
      var idx = this.parent.children.indexOf(this);
      if (idx > -1) this.parent.children.splice(idx, 1);
      this.parent.children.push(this);
      
      // set the child's container-div
      this.containerDiv = this.parent.containerDiv; 
      
      // add to new parent's container
      var indent = (parseInt(this.parent.commentDiv.style.marginLeft) || 0) + 50;
      console.log(this.parent.commentDiv.style.marginLeft);
      this.commentDiv.style.marginLeft = indent + "px";
      this.containerDiv.insertBefore(this.commentDiv,this.parent.commentDiv.nextSibling);
    } else {
      // create new new container div.
      // no farther work has to be done.
      this.containerDiv = document.createElement('div');
      this.containerDiv.className = 'comment-container';
      this.containerDiv.appendChild(this.commentDiv);
    }
  }
})

Comment.div = document.body.appendChild(document.createElement('div'));
Comment.rootEditor = new Editor(false, null);

Comment.loadComments = function() {
  sendHTTP('GET', '/posts/' + postId + '/comments.json', {}, function(http) {
    if (http.status == 200) {
      Comment.div.appendChild(Comment.rootEditor.el);
      
      var comments = JSON.parse(http.responseText);
      
      for (var i = 0; i < comments.length; i++) {
        var c = new Comment(comments[i], null);
        Comment.div.appendChild(c.containerDiv);
      }
    }
  });
}

window.onload = function() {
  var css = document.createElement('link');
  css.rel = 'stylesheet';
  css.type = 'text/css';
  css.href = '/assets/css/comments.css';
  document.body.appendChild(css);
  Comment.loadComments();
}