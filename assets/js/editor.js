var body = document.getElementById('editor');
var oldkeydown = body.onkeydown;

body.onkeydown = function(e) {
  if (oldkeydown) oldkeydown(e);
  if (e.ctrlKey && e.key === "i") {
    document.execCommand('insertText', false, '*' + body.value.substring(body.selectionStart, body.selectionEnd) + '*');
    return false;
  } else if (e.ctrlKey && e.key === "b") {
    document.execCommand('insertText', false, '**' + body.value.substring(body.selectionStart, body.selectionEnd) + '**');
    return false;
  }
};

var i = document.getElementById('input-tags');
body.parentNode.insertBefore((new WordList(i)).div, body.nextSibling);
