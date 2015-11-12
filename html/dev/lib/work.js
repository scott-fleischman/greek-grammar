import React from 'react';
import R from 'ramda';
import { OverlayTrigger, Button, Popover } from 'react-bootstrap';
import { Property } from './property.js';

const Word = ({ word, key, wordSummary, url, getTypeTitle, getValueTitle, getValueListUrl, getInstanceListUrl }) => {
  const getTypeIndex = x => x[0];
  const getValueIndex = x => x[1];
  const getPropertyTitle = x => getTypeTitle(getTypeIndex(x));
  const getPropertyValue = x => getValueTitle(getTypeIndex(x), getValueIndex(x));
  const summaryProperties = R.compose(R.sortBy(x => wordSummary.get(getTypeIndex(x))), R.filter(x => wordSummary.has(getTypeIndex(x)))) (word);

  const popoverList = R.addIndex(R.map)
    ((x, i) => (
      <Property
        key={i}
        name={getPropertyTitle(x)}
        value={getPropertyValue(x)}
        nameUrl={getValueListUrl(getTypeIndex(x))}
        valueUrl={getInstanceListUrl(getTypeIndex(x), getValueIndex(x))}
      />
    ))
    (summaryProperties);
  const popover = (
    <Popover id={'wordProperties.' + key} title="Properties" style={{maxWidth: '100%'}}>
      <div><a href={url}>View Details</a></div>
      {popoverList}
    </Popover>
  );
  return (
    <OverlayTrigger trigger="click" rootClose placement="bottom" overlay={popover}>
      <span><a className="workWord" href="javascript:">{getPropertyValue(word[0])}</a> </span>
    </OverlayTrigger>
  );
}

const WordGroup = ({ key, wordIndexes, words, workIndex, wordSummary, getWordUrl, getTypeTitle, getValueTitle, getValueListUrl, getInstanceListUrl }) => {
  const wordElements = R.map
    (x => (
      <Word
        key={key + '.' + x}
        word={words[x]}
        wordSummary={wordSummary}
        url={getWordUrl(workIndex, x)}
        getTypeTitle={getTypeTitle}
        getValueTitle={getValueTitle}
        getValueListUrl={getValueListUrl}
        getInstanceListUrl={getInstanceListUrl}
      />))
    (wordIndexes);
  return (
    <div className="wordGroup">
      {wordElements}
    </div>
  );
}

export const Work = ({ work, workIndex, getWordUrl, getTypeTitle, getValueTitle, getValueListUrl, getInstanceListUrl }) => {
  const wordSummary = new Map(R.addIndex(R.map) ((x, i) => [x, i]) (work.wordSummary));
  const wordGroups = R.addIndex(R.map)
    ((wg, i) => (
      <WordGroup
        key={workIndex + '.' + i}
        wordIndexes={wg}
        words={work.words}
        workIndex={workIndex}
        wordSummary={wordSummary}
        getWordUrl={getWordUrl}
        getTypeTitle={getTypeTitle}
        getValueTitle={getValueTitle}
        getValueListUrl={getValueListUrl}
        getInstanceListUrl={getInstanceListUrl}
      />))
    (work.wordGroups[0].words);
  return (
    <div className="workContainer">
      { wordGroups }
    </div>
  );
};
