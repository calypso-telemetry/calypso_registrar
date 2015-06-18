-author("begemot").

-record(gps, {
  x :: integer(),
  y :: integer(),
  z :: integer(),
  precision :: integer(),
  geohash :: binary()
}).

-define(IS_GPS(Gps), is_record(Gps, gps)).

-define(GPS_SCALE, 10000000).
-define(GPS_ENSCALE(Value), round(Value * ?GPS_SCALE)).
-define(GPS_DESCALE(Value), Value / ?GPS_SCALE).
