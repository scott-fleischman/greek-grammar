import React from 'react';
import R from 'ramda';

const InstanceInfo = ({ getWorkInfo, getTypeInfo, workIndex, wordIndex, url, getInstanceListUrl }) => {
  const workInfo = getWorkInfo(workIndex);
  const wordInfo = workInfo.wordInfos[wordIndex];
  const wordValues = wordInfo.v;
  const wordContextIndex = wordInfo.c;
  const wordProperties = R.map
    (v => ({
      title: getTypeInfo(v[0]).values[v[1]].t,
      url: getInstanceListUrl(v[0], v[1]),
    }))
    (wordValues);
  const wordInfoElements = R.addIndex(R.map)
    ((x, i) => (<span key={i} className="instanceInfo"> &mdash; <a className="instanceInfo" href={x.url}>{x.title}</a></span>))
    (wordProperties.slice(1));
  return (
    <div>
      <a href={url}>{wordProperties[0].title}</a>
      {wordInfoElements}
    </div>
  );
};

export const InstanceList = ({ getWorkInfo, getTypeInfo, instances, typeIndex, valueIndex, getWordUrl, getInstanceListUrl, getShowItemInfo }) => {
  const indexedInstances = R.addIndex(R.map) ((x, i) => ({ instanceIndex: i, workIndex: x[0], wordIndex: x[1] })) (instances);
  const { shownItems, showAllButton } = getShowItemInfo(2000, indexedInstances);
  return (
    <div className="listContainer">
      {R.map (x => (
        <InstanceInfo
          key={typeIndex + '.' + valueIndex + '.' + x.instanceIndex}
          getWorkInfo={getWorkInfo}
          getTypeInfo={getTypeInfo}
          workIndex={x.workIndex}
          wordIndex={x.wordIndex}
          url={getWordUrl(x.workIndex, x.wordIndex)}
          getInstanceListUrl={getInstanceListUrl}
        />
      )) (shownItems)}

      { showAllButton }
    </div>
  );
};
