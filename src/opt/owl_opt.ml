module S = struct
   module Gd = Gd_s
   module Adam = Adam_s
   module Rmsprop = Rmsprop_s
end

module D = struct
   module Gd = Gd_d
   module Adam = Adam_d
   module Rmsprop = Rmsprop_d
end
module Prms = Prms
