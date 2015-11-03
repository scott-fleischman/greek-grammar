import R from 'ramda';
import * as actions from "./actions.js"
import { combineReducers } from 'redux'

export const viewState = R.compose(R.fromPairs, R.map(x => [x,x])) ([
	'loadingIndex',
	'workList',
]);

const requestIndex = state => ({ view: viewState.loadingIndex });
const receiveIndex = (state, index) => ({ view: viewState.workList, index: index });

export function root(state = {}, action) {
	switch (action.type) {
		case actions.types.requestIndex: return requestIndex(state);
		case actions.types.receiveIndex: return receiveIndex(state, action.index);
		default: return state;
	}
}
