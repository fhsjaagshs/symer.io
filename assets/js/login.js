var passwd = document.getElementById('password');
var uname = document.getElementById('username');
var form = document.getElementById('loginform');
var submit = document.getElementById('submit');
uname.onkeydown = function(e) { if (e.keyCode == 13) passwd.focus(); }
passwd.onkeydown = function(e) { if (e.keyCode == 13) submit.click(); }
submit.onclick = function() { form.submit(); }