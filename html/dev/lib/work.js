import React from 'react';
import R from 'ramda';
import { OverlayTrigger, Button, Popover } from 'react-bootstrap';
import { SingleProperty } from './property.js';

const Word = ({ word, key, wordSummary, specialTypes, url, getTypeTitle, getValueTitle, getValueListUrl, getInstanceListUrl }) => {
  const getTypeIndex = x => x[0];
  const getValueIndexes = x => x[1];
  const getPropertyTitle = x => getTypeTitle(getTypeIndex(x));
  const summaryProperties = R.compose(R.sortBy(x => wordSummary.get(getTypeIndex(x))), R.filter(x => wordSummary.has(getTypeIndex(x)))) (word);
  const surfaceWordProperty = word[0];

  const getWordTypeValue = i => {
    const match = R.filter(x => x[0] === i)(word);
    if (!match.length) {
      return undefined;
    }

    const value = getValueTitle(match[0][0], match[0][1][0]);
    if (value === 'No prefix')
      return undefined;
    else if (value === 'No suffix')
      return undefined;
    else
      return value;
  }

  const surfaceValue = getWordTypeValue(specialTypes.surface);
  const prefixValue = getWordTypeValue(specialTypes.prefix);
  const suffixValue = getWordTypeValue(specialTypes.suffix);

  const popoverList = R.addIndex(R.map)
    ((x, i) => (
      <SingleProperty
        key={key + '.' + i}
        name={getPropertyTitle(x)}
        nameUrl={getValueListUrl(getTypeIndex(x))}
        valueIndexes={getValueIndexes(x)}
        getValue={v => getValueTitle(getTypeIndex(x), v)}
        getValueUrl={v => getInstanceListUrl(getTypeIndex(x), v)}
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
    <span>
      <span className="workWordPrefix">{prefixValue}</span>
      <OverlayTrigger trigger="click" rootClose placement="bottom" overlay={popover}>
        <a className="workWord" href="javascript:">{surfaceValue}</a>
      </OverlayTrigger>
      <span className="workWordSuffix">{suffixValue}</span>
      <span> </span>
    </span>
  );
}

const WordGroup = ({ key, wordIndexes, words, workIndex, wordSummary, specialTypes, getWordUrl, getTypeTitle, getValueTitle, getValueListUrl, getInstanceListUrl }) => {
  const wordElements = R.map
    (x => (
      <Word
        key={key + '.' + x}
        word={words[x]}
        wordSummary={wordSummary}
        specialTypes={specialTypes}
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

export const Work = ({ work, workIndex, specialTypes, getWordUrl, getTypeTitle, getValueTitle, getValueListUrl, getInstanceListUrl, getShowItemInfo }) => {
  const wordSummary = new Map(R.addIndex(R.map) ((x, i) => [x, i]) (work.wordSummary));
  const { shownItems, showAllButton } = getShowItemInfo(20, work.wordGroups[0].words);
  const wordGroups = R.addIndex(R.map)
    ((wg, i) => (
      <WordGroup
        key={workIndex + '.' + i}
        wordIndexes={wg}
        words={work.words}
        workIndex={workIndex}
        wordSummary={wordSummary}
        specialTypes={specialTypes}
        getWordUrl={getWordUrl}
        getTypeTitle={getTypeTitle}
        getValueTitle={getValueTitle}
        getValueListUrl={getValueListUrl}
        getInstanceListUrl={getInstanceListUrl}
      />))
    (shownItems);
  return (
    <div className="workContainer">
      { wordGroups }
      { showAllButton }
    </div>
  );
};
