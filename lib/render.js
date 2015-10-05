import _ from 'lodash';
import $ from 'jquery';
import queryString from 'query-string';

function renderList(data) {
	const element = $('div.main');
	_.forEach(data.items, function(item) {
		element.append($('<p></p>').text(item.title));
	});
}

function renderGroup(data) {
	const element = $('#accordion');
	_.forEach(data.items, function(item) {
		const itemIndex = item.index;
 		const headingId = 'heading' + itemIndex;
 		const collapseId = 'collapse' + itemIndex;

 		const itemElement = $('<div class="panel panel-default"></div>');

	 	const headingElement = $(`<div class="panel-heading" role="tab" id="${headingId}"></div>`);
	 	const titleElement = $('<h4 class="panel-title"></h4>');
	 	const link = $(`<a class="collapsed" role="button" data-toggle="collapse" data-parent="#accordion" href="#${collapseId}" aria-expanded="false" aria-controls="${collapseId}">`);
	 	link.text(item.title);
	 	titleElement.append(link);
	 	headingElement.append(titleElement);
	 	itemElement.append(headingElement);

	 	const collapseElement = $(`<div id="${collapseId}" class="panel-collapse collapse" role="tabpanel" aria-labelledby="${headingId}"></div>`);
	 	const bodyElement = $('<ul class="list-group"></ul>');
        _.forEach(_.take(item.words, 100), function(word) {
		 	const bodyItemElement = $('<li class="list-group-item"></li>');
		 	bodyItemElement.text(word);
		 	bodyElement.append(bodyItemElement);
        });

	 	collapseElement.append(bodyElement);
	 	itemElement.append(collapseElement);

	 	element.append(itemElement);
	});
}

export function render() {
	const result = queryString.parse(window.location.search);
	console.log(result);
	$.getJSON('data/work.json', renderGroup);
}
