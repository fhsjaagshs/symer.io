"use strict";

var postId = +document.getElementsByClassName('post-title')[0].id;

/*
  method (String) The HTTP method to use
  url (String) duh
  params (Object) An object containing the parameters (eg {myparam: "myvalue"})
  callback (Function) A function that will be called with the XMLHTTPRequest object (eg function(http) { ... })
*/
function sendHTTP(method, url, params, callback) {
  var pairs = [];
  
  for (var k in params) {
    if (params.hasOwnProperty(k)) {
      pairs.push(k + '=' + params[k]);
    }
  }
  
  var http = new XMLHttpRequest();
  http.open(method, url, true);
  http.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
  http.onreadystatechange = function() { 
    if (method == "HEAD" && http.readyState == 2) {
      if (callback) callback(http);
    } else if (http.readyState == 4) {
      if (callback) callback(http);
    }
  }
  
  return http.send(pairs.join('&'));
}

function CommentField(aPlaceholder) {
  var el = document.createElement('input');
  el.type = 'text';
  el.className = 'comment-field textfield';
  el.placeholder = aPlaceholder;
  return el;
}

function CommentTextarea(aPlaceholder) {
  var el = document.createElement('textarea');
  el.className = 'comment-textarea textarea';
  el.placeholder = aPlaceholder;
  return el;
}

function GoButton(editor, cb) {
  var b = document.createElement('div');
  b.className = 'gobutton';
  b.onclick = function() {
    var params = {
      email: editor.email,
      display_name: editor.name,
      body: editor.commentBody
    };
    
    if (editor.comment) {
      params.parent_id = editor.comment.id;
      editor.hide();
    }
    
    editor.reset();
    
    sendHTTP('POST', '/posts/' + postId + '/comments', params, function(http) {
      if (http.status == 200) {
        var c = new Comment(JSON.parse(http.responseText), null);
        if (editor.comment) editor.comment.appendChild(c);
        else Comment.div.appendChild(c.containerDiv);
        if (cb) cb(true);
      } else {
        if (cb) cb(false);
      }
    });
  };
  return b;
}

// TODO: make this a form?
function Editor(hidden, comment, callback) {
  this.comment = comment;
  this.el = document.createElement('div');
  this.el.className = 'editor';
  this.el.style.display = hidden?'none':'block';
  this.el.style.textAlign = "center";
  
  this.nameField = new CommentField('Name');
  this.emailField = new CommentField('Email (not displayed)');
  this.textarea = new CommentTextarea('Speak your mind...');
  this.button = new GoButton(this, callback);
  
  this.el.appendChild(this.nameField);
  this.el.appendChild(this.emailField);
  this.el.appendChild(this.textarea);
  this.el.appendChild(this.button);
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
};

Object.defineProperty(Editor.prototype, 'commentBody', {
    get: function() { return this.textarea.value; },
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

function Comment(json) {
  this.commentDiv = document.createElement('div');
  this.commentDiv.className = 'comment';
  
  this.commentName = document.createElement('h3');
  this.commentName.className = 'comment-name';
  
  this.commentBody = document.createElement('p');
  this.commentBody.className = 'comment-body';
  
  var button = document.createElement('a');
  var editor = new Editor(true, this, function(succeeded) {
    if (!succeeded) alert('failed to post comment.');
    button.innerHTML = 'Reply';
  });
  
  this.button = button;
  this.button.className = 'button';
  this.button.innerHTML = 'Reply';
  this.button.onclick = function() {
    if (editor.isVisible()) {
      this.innerHTML = 'Reply';
      editor.hide();
    } else {
      this.innerHTML = 'Cancel';
      editor.show();
    }
  };

  this.commentDiv.appendChild(this.commentName);
  this.commentDiv.appendChild(this.commentBody);
  this.commentDiv.appendChild(this.button);
  this.commentDiv.appendChild(editor.el);
  
  this.id = json.id;
  this.children = [];
  this.parent = null;
  this.displayName = json.display_name;
  this.email = json.email;
  this.body = json.body;
  this.timestamp = json.timestamp;

  for (var i = 0; i < json.children.length; i++) {
    var c = new Comment(json.children[i]);
    c.parent = this;
  }
}

Object.prototype.insertAfter = function (newNode) { this.parentNode.insertBefore(newNode, this.nextSibling); };

Comment.prototype = {
  appendChild: function(child) { child.parent = this; }
};

Object.defineProperty(Comment.prototype, 'displayName', {
  get: function() { return this.commentName.innerHTML; },
  set: function(v) { this.commentName.innerHTML = v; }
});

Object.defineProperty(Comment.prototype, 'body', {
  get: function() { return this.commentBody.innerHTML; },
  set: function(v) { this.commentBody.innerHTML = v; }
});

Object.defineProperty(Comment.prototype, 'parent', {
  enumerable: true,
  writeable: true,
  get: function() { return this.__parent || null; },
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
    } else {
      // create new new container div.
      // no farther work has to be done.
      this.containerDiv = document.createElement('div');
      this.containerDiv.className = 'comment-container';
    }
  }
});

Object.defineProperty(Comment.prototype, 'containerDiv', {
  enumerable: true,
  writeable: true,
  get: function() { return this.__containerDiv || null; },
  set: function(div) {
    // remove from parent node
    if (this.containerDiv && this.commentDiv.parentNode == this.containerDiv) {
      this.containerDiv.removeChild(this.commentDiv);
    }
    
    // set the property
    this.__containerDiv = div;
    
    // add the commentDiv to the containerDiv
    if (this.parent) {
      var indent = (parseInt(this.parent.commentDiv.style.marginLeft) || 0) + 50;
      this.commentDiv.style.marginLeft = indent + "px";
      this.containerDiv.insertBefore(this.commentDiv,this.parent.commentDiv.nextSibling);
    } else {
      this.containerDiv.appendChild(this.commentDiv);
    }

    // recurse over children
    for (var i = 0; i < this.children.length; i++) {
      this.children[i].containerDiv = div;
    }
  }
});

Comment.div = document.body.appendChild(document.createElement('div'));
Comment.rootEditor = new Editor(false, null, null);

Comment.loadComments = function() {
  sendHTTP('GET', '/posts/' + postId + '/comments.json', {}, function(http) {
    if (http.status == 200) {
      Comment.div.appendChild(Comment.rootEditor.el);
      
      var comments = JSON.parse(http.responseText);
      
      for (var i = 0; i < comments.length; i++) {
        Comment.div.appendChild((new Comment(comments[i])).containerDiv);
      }
    }
  });
};

window.onload = function() {
  var css = document.createElement('link');
  css.rel = 'stylesheet';
  css.type = 'text/css';
  css.href = '/assets/css/comments.css';
  document.body.appendChild(css);
  Comment.loadComments();
};