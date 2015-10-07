import _ from 'lodash';
import $ from 'jquery';
import queryString from 'query-string';

function renderGroup(dataName, element, process) {
	return data => {
		const processedItems = process(data.items);
		_.forEach(processedItems, item => {
			const itemIndex = item.index;
	 		const headingId = 'heading' + itemIndex;
	 		const collapseId = 'collapse' + itemIndex;
	 		const elementId = element.id;

	 		const itemElement = $('<div class="panel panel-default"></div>');

		 	const headingElement = $(`<div class="panel-heading" role="tab" id="${headingId}"></div>`);
		 	const titleElement = $('<h4 class="panel-title"></h4>');
		 	const link = $(`<a class="collapsed" role="button" data-toggle="collapse" data-parent="${elementId}" href="#${collapseId}" aria-expanded="false" aria-controls="${collapseId}">`);
		 	link.text(item.title);
		 	titleElement.append(link);
		 	headingElement.append(titleElement);
		 	itemElement.append(headingElement);

		 	const collapseElement = $(`<div id="${collapseId}" class="panel-collapse collapse" role="tabpanel" aria-labelledby="${headingId}"></div>`);
		 	const bodyElement = $('<ul class="list-group"></ul>');
	        _.forEach(_.take(item.words, 100), word => {
			 	const bodyItemElement = $('<li class="list-group-item"></li>');
			 	bodyItemElement.text(word);
			 	bodyElement.append(bodyItemElement);
	        });

		 	collapseElement.append(bodyElement);
		 	itemElement.append(collapseElement);

		 	element.append(itemElement);
		});
	}
}

function doWhere(whereName, whereId) {
	return items => _.filter(items, x => x[whereName] === whereId);
}

export function render() {
	const element = $('#main');
	const query = queryString.parse(window.location.search);
	let dataName = query.d || 'work';
	let process = x => x;
	if (query.where && query.whereId) {
		const whereName = query.where;
		const whereId = parseInt(query.whereId);
		if (!isNaN(whereId))
			process = _.flow(process, doWhere(whereName, whereId))
	}
	$.getJSON(`data/${dataName}.json`, renderGroup(dataName, element, process));
}
