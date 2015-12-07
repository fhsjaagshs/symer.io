// WordList
// constructor build a WordList
// words -> words to display in the wordlist (cloned)
var WordList = function WordList(input) {
  this.tagDivs = []; // type: Array of HTMLElement
  this.formInput = input; // hook into HTML forms
  this.words = input.value.split(",");

  this.div = document.createElement("div");
  this.div.className = "wordlist-view";

  this.input = document.createElement("input");
  this.input.type = "text";
  this.input.placeholder = "tag";
  this.input.wordlist = this;
  this.input.onkeyup = function(e) {
    if (e.keyCode == 13 || e.keyCode == 44) { // enter key
      if (this.wordlist.addWord(this.value)) {
        this.value = "";
      }
    }
  }
  this.div.appendChild(this.input);
  this.reloadWords();
}

WordList.prototype = {
  addWord: function(word) {
    var wordT = word.trim();
    if (wordT && wordT.length > 0 && this.words.indexOf(wordT) == -1) {
      this.words.push(wordT);
      this.reloadWords();
      if (this.onAdd != undefined) this.onAdd(wordT);
      return true;
    }
    return false;
  },
  deleteWord: function(word) {
    var wordT = word.trim();
    var idx = this.words.indexOf(wordT);
    if (idx != -1) {
      this.words.splice(idx, 1);
      this.reloadWords();
      if (this.onDelete != undefined) this.onDelete(wordT);
    }
  },
  reloadWords: function() {
    this.tagDivs.forEach(function(node) {
      node.parentNode.removeChild(node);
    }, this);
    
    this.tagDivs = [];
    
    this.words.forEach(function(word) {
      this.tagDivs.push(this.divForWord(word));
    }, this);
    
    this.tagDivs.forEach(function(node) {
      this.div.insertBefore(node, this.input);
    }, this);
    
    this.formInput.value = this.words.join(',');
  },
  divForWord: function(word) {
    var div = document.createElement("div");
    div.className = "wordlist-item";
    div.wordlist = this;
    div.onclick = function(e) {
      var spanWidth = this.offsetWidth;
      var spanXCoord = 0;
      
      var el = this;
      while (el && !isNaN(el.offsetLeft)) {
        spanXCoord += el.offsetLeft-el.scrollLeft;
        el = el.offsetParent;
      }
      
      if (e.clientX > spanWidth-25 + spanXCoord) {
        this.wordlist.deleteWord(word);
      }
      
      e.stopPropagation();
    }

    var span = document.createElement("span");
    span.innerHTML = word.trim();
    div.appendChild(span);
    
    return div;
  }
}