import React from 'react';
import R from 'ramda';
import { Button } from 'react-bootstrap';

const WorkInfo = ({ title, source, wordCount }) => (
	<div>
		<span className="workInfoSource">{source}</span> <Button bsStyle="link" className="workInfoButton">{title}</Button> &mdash; <span>{wordCount} words</span>
	</div>
);

export const WorkList = ({ works }) => (
	<div className="workListContainer">
		{R.addIndex(R.map) ((x, i) => (<WorkInfo key={i} {...x} />)) (works)}
	</div>
);
