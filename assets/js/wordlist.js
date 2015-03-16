$.fn.wordlist = function () {
  return this.each(function() {
  	var _self = $(this),
        _view  = $('<div class="wordlist-view" />'),
        _input = $('<input type="text" placeholder="tag" />'),
  		  _label = $(this).parents(".controls").siblings(".control-label").children("label");
  
    var words = (_self.val().length == 0) ? [] : $.map(_self.val().split(','), function(obj,i) { return obj.trim(); });
    var spans = []
        
    var refresh = function() {
      _self.val(words.join(','));
      _view.width = _view.children
      
      $.each(spans, function(i,span) { span.remove(); });
      spans = $.map(words, function(word) { return $("<div class=\"wordlist-item\"><span>"+word.trim()+"</span></div>") });
      $.each(spans, function (i,span) { span.insertBefore(_input); });
    };
    
    var addWord = function(word) {
      if (word && word.length > 0 && words.indexOf(word) == -1) {
        words.push(word.trim());
        refresh();
        _self.trigger("wordlist:addedWord",[word]);
        return true;
      }
      return false;
    }
    
    var deleteWord = function(word) {
      words.splice(words.indexOf(word), 1);
      refresh();
      _self.trigger("wordlist:deletedWord",[word]);
      return true;
    }
  
    _view.insertAfter(_self);
  	_self.hide();
    _input.appendTo(_view);
    refresh();

    _view.on("click","span",function(e) {
      if (e.pageX > $(e.target).outerWidth()-25 + $(e.target).position().left) {
        deleteWord($(e.target).text().trim());
      }
    });
    
		_label.click(function(e){
			e.preventDefault();
			_input.focus();
		});
    
  	_input.on("keypress", function(e) {
      if (e.which == 13 || e.which == 44) { // enter key
        var word = _input.val().trim();
        if (addWord(word)) {
  				e.preventDefault();
  				e.stopPropagation();
          _input.val("");
        }
      }
  	});
  });
}