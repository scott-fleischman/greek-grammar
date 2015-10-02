import _ from 'lodash';
import $ from 'jquery';

function renderList(data) {
	var element = $('div.main');
	_.forEach(data.items, function(item) {
		element.append($('<p></p>').text(item.title));
	});
}

function renderGroup(data) {
	var element = $('#accordion');
	_.forEach(data.items, function(item) {
		var itemIndex = item.index;
 		var headingId = 'heading' + itemIndex;
 		var collapseId = 'collapse' + itemIndex;

 		var itemElement = $('<div class="panel panel-default"></div>');

	 	var headingElement = $('<div class="panel-heading" role="tab" id="' + headingId + '"></div>');
	 	var titleElement = $('<h4 class="panel-title"></h4>');
	 	var link = $('<a class="collapsed" role="button" data-toggle="collapse" data-parent="#accordion" href="#' + collapseId + '" aria-expanded="false" aria-controls="' + collapseId + '">');
	 	link.text(item.title);
	 	titleElement.append(link);
	 	headingElement.append(titleElement);
	 	itemElement.append(headingElement);

	 	var collapseElement = $('<div id="' + collapseId + '" class="panel-collapse collapse" role="tabpanel" aria-labelledby="' + headingId + '"></div>');
	 	var bodyElement = $('<ul class="list-group"></ul>');
        _.forEach(_.take(item.words, 100), function(word) {
		 	var bodyItemElement = $('<li class="list-group-item">Bootply</li>');
		 	bodyItemElement.text(word);
		 	bodyElement.append(bodyItemElement);
        });

	 	collapseElement.append(bodyElement);
	 	itemElement.append(collapseElement);

	 	element.append(itemElement);
	});
}

export function render() {
	$.getJSON('data/work.json', renderGroup);
}
