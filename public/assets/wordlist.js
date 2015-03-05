$.fn.wordlist = function () {
  return this.each(function() {
  	var _self = $(this),
        _view  = $('<div class="wordlist-view" />'),
        _input = $('<input type="text" placeholder="tag"/>'),
  		  _label = $(this).parents(".controls").siblings(".control-label").children("label");
  
    var words = (_self.val().length == 0) ? [] : $.map(_self.val().split(','), function(obj,i) { return obj.trim(); });
    var spans = []
        
    var refresh = function() {
      _self.val(words.join(','));
      
      $.each(spans, function(i,span) { span.remove(); });
      spans = $.map(words, function(word) { return $("<div class=\"wordlist-item\"><span>"+word.trim()+"</span></div>") });
      $.each(spans, function (i,span) { span.insertBefore(_input); });
    };
    
    var addWord = function(word) {
      if (word != undefined && word != null && word != "") {
        words.push(word.trim());
        refresh();
        _self.trigger("wordlist:addedWord",[word]);
      }
    }
    
    var deleteWord = function(word) {
      words.splice(words.indexOf(word), 1);
      refresh();
      _self.trigger("wordlist:deletedWord",[word]);
    }
  
    _view.insertAfter(_self);
  	_self.hide();
    _input.appendTo(_view);
    refresh();

    _view.on("click","span",function(e) {
      console.log("width: " + e.target.offsetWidth + "offset: " + e.clientX);
   //   if (e.offsetX > e.target.offsetWidth) {
        deleteWord($(e.target).text().trim());
        refresh();
    //  }
    });
    
		_label.click(function(e){
			e.preventDefault();
			_input.focus();
		});
    
  	_input.on("keypress", function(e) {
      if (e.which == 13 || e.which == 44) { // enter key
        var word = _input.val().trim();
        _input.val("");
        if (word.length > 0) {
  				e.preventDefault();
  				e.stopPropagation();
  				
          addWord(word);
        }
      }
  	});
  });
}