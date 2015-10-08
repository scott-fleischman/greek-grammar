import R from 'ramda';
import $ from 'jquery';
import queryString from 'query-string';
import 'fetch';

function renderGroup(element, groupName, data) {
	data.items.forEach(item => {
		const itemIndex = item.index;
		const headingId = 'heading' + itemIndex;
		const collapseId = 'collapse' + itemIndex;
		const elementId = element.id;

		const itemElement = $('<div class="panel panel-default"></div>');

		const headingElement = $(`<div class="panel-heading" role="tab" id="${headingId}"></div>`);
		const titleElement = $('<h4 class="panel-title"></h4>');
		const link = $(`<a class="collapsed" role="button" data-toggle="collapse" data-parent="${elementId}" href="#${collapseId}" aria-expanded="false" aria-controls="${collapseId}">`);
		let linkText = data.data[groupName][item.key].title;
		const count = item.values.length;
		if (count !== 1)
			linkText = linkText + ' ';
		link.text(linkText);
		if (count !== 1)
			link.append(renderCount(item.values.length));
		titleElement.append(link);
		headingElement.append(titleElement);
		itemElement.append(headingElement);

		const collapseElement = $(`<div id="${collapseId}" class="panel-collapse collapse" role="tabpanel" aria-labelledby="${headingId}"></div>`);
		const bodyElement = $('<ul class="list-group"></ul>');
		item.values.forEach(value => {
			const bodyItemElement = $('<li class="list-group-item"></li>');
			bodyItemElement.text(value.title);
			bodyElement.append(bodyItemElement);
		});

		collapseElement.append(bodyElement);
		itemElement.append(collapseElement);

		element.append(itemElement);
	});
}

function renderCount(count) {
	return $(`<span class="badge">${count}</span>`);
}

function doGroup(groupName) {
	return R.compose(
		R.last,
		R.mapAccum((i, x) => [i + 1, { index: i, key: x.key, values: x.values } ], 0),
		xs => R.map(x => ({ key: x, values: xs[x] }), R.keys(xs)),
		R.groupBy(x => x[groupName])
		);
}

function checkStatus(response) {
  if (response.status >= 200 && response.status < 300) {
    return response;
  } else {
    const error = new Error(response.statusText);
    error.response = response;
    throw error;
  }
}

function loadData(dataName) {
	return fetch(`data/${dataName}.json`)
		.then(checkStatus)
		.then(x => x.json())
		.catch(x => console.log('Unable to load data', dataName, x));
}

function loadCombinedData(dataNames) {
	const promises = R.map(loadData, dataNames);
	return Promise.all(promises)
		.then(values => 
			{
				let result = {};
				values.forEach(x => result[x.name] = x.items);
				console.log(result);
				return result;
			});
}

export function render() {
	const element = $('#main');
	const query = queryString.parse(window.location.search);
	let dataName = query.view || 'work';

	let process = x => x;

	if (query.where && query.whereId) {
		const whereName = query.where;
		const whereId = parseInt(query.whereId);
		if (!isNaN(whereId))
			process = R.compose(R.filter(x => x[whereName] === whereId), process);
	}

	const groupName = query.group || dataName;
	process = R.compose(doGroup(groupName), process);

	const doRender = R.curry(renderGroup)(element, groupName);
	const callback = R.compose(doRender, x => ({ items: process(x[dataName]), data: x }));
	loadCombinedData(['word','work']).then(callback);
}
