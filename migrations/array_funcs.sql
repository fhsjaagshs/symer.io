CREATE OR REPLACE FUNCTION array_diff(anyarray,anyarray) RETURNS anyarray AS $BODY$
BEGIN
  SELECT array_agg(unnest) FROM unnest($1) WHERE unnest != ALL($2) INTO $1;
  return $1;
END;
$BODY$ LANGUAGE 'plpgsql' VOLATILE;

-- uniq(myarray)
-- returns unique copy of myarray
CREATE OR REPLACE FUNCTION uniq(anyarray) RETURNS anyarray AS $BODY$
BEGIN
  SELECT ARRAY(SELECT DISTINCT unnest FROM unnest($1)) INTO $1;
  return $1;
END;
$BODY$ LANGUAGE 'plpgsql' VOLATILE;

-- uniq_append(array,elem)
-- appends elem onto array if it doesn't exist in array.
CREATE OR REPLACE FUNCTION uniq_append(anyarray, anyelement) RETURNS anyarray AS $BODY$
BEGIN
	if ($2 = ANY($1)) THEN
		return $1;
	ELSE
		return array_append($1, $2);
	END IF;
END;
$BODY$ LANGUAGE 'plpgsql' VOLATILE;

-- uniq_prepend(array,elem)
-- prepends elem onto array if it doesn't exist in array.
CREATE OR REPLACE FUNCTION uniq_prepend(anyarray, anyelement) RETURNS anyarray AS $BODY$
BEGIN
	if ($2 = ANY($1)) THEN
		return $1;
	ELSE
		return array_prepend($1, $2);
	END IF;
END;
$BODY$ LANGUAGE 'plpgsql' VOLATILE;

-- uniq_cat(one, two)
-- Concatenates one and an array consisting of values in two that don't exist in one
CREATE OR REPLACE FUNCTION uniq_cat(anyarray, anyarray) RETURNS anyarray AS $BODY$
BEGIN
  return array_cat($1,array_diff($2,$1));
END;
$BODY$ LANGUAGE 'plpgsql' VOLATILE;



-- uniq_cat2(one, two)
-- Concatenates one and two and returns a copy without duplicates.
CREATE OR REPLACE FUNCTION uniq_cat2(anyarray, anyarray) RETURNS anyarray AS $BODY$
BEGIN
  return uniq(array_cat($1,$2));
END;
$BODY$ LANGUAGE 'plpgsql' VOLATILE;