import * as actions from "./actions.js"
import { combineReducers } from 'redux'

const viewState = {
	loadingIndex: 'loadingIndex',
	loadingWork: 'loadingWork',
};

const requestIndex = state => ({ view: 'loading' });
const receiveIndex = (state, index) => ({ view: 'workList', index: index });

export function root(state = {}, action) {
	switch (action.type) {
		case actions.types.requestIndex: return requestIndex(state);
		case actions.types.receiveIndex: return receiveIndex(state, action.index);
		default: return state;
	}
}
