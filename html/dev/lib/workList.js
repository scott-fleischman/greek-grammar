import React from 'react';
import R from 'ramda';
import { Button } from 'react-bootstrap';

const WorkInfo = ({ title, source, wordCount, workIndex, viewWork }) => (
	<div>
		<span className="workInfoSource">{source}</span>
		&ensp;
		<Button bsStyle="link" className="workInfoButton" onClick={() => viewWork(workIndex)}>{title}</Button>
		&ensp;
		<span>{wordCount} words</span>
	</div>
);

export const WorkList = ({ works, viewWork }) => (
	<div className="workListContainer">
		{R.addIndex(R.map) ((x, i) => (<WorkInfo key={i} viewWork={viewWork} workIndex={i} {...x} />)) (works)}
	</div>
);
