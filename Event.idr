module Event

-- data MetaInfo = String

%elim data Event =
  Click |
  Submit |
  Meta String Event
