import React from 'react';
import R from 'ramda';
import { Button } from 'react-bootstrap';

const ShowAllButton = ({showAllLoading, handleShowAll}) => {
  const text = showAllLoading ? 'Loading…' : 'Show all (may take a while)';
  return (
    <div className="showAllButton">
      <Button onClick={() => handleShowAll()}>{text}</Button>
    </div>
  );
};

export const getShowAllInfo = (initialCount, items, showAllLoading, showAllItems, handleShowAll) => {
  const shouldShowAll = initialCount + (initialCount / 5) >= items.length || showAllItems ;
  const shownItems = shouldShowAll ? items : R.take(initialCount)(items);
  const showAllButton = shouldShowAll ? undefined : (<ShowAllButton showAllLoading={showAllLoading} handleShowAll={handleShowAll} />);
  return {
    shownItems: shownItems,
    showAllButton: showAllButton,
  };
};
