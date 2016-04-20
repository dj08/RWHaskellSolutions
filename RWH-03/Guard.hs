-- file: ch03/Guard.hs
-- unwrap Maybe wrapper
fromMaybe defval wrapped =
  case wrapped of
   Nothing    -> defval
   Just value -> value
