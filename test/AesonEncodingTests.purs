module AesonEncodingTests (all) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Generic (class Decode, class Encode, Options, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Foreign.Generic.Class (aesonSumEncoding)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import TestUtils (testRoundTrip)

all :: TestSuite
all =
  suite "Aeson Encoding" do
    suite "Records" do
      test "Record" do
        equal "{\"name\":\"Tester\",\"age\":13}"
          (encodeJSON (ARecord { name: "Tester", age: 13 }))
      testRoundTrip (ARecord { name: "Tester", age: 13 })
    suite "Sum Types" do
      test "Sum type, no arg constructor." do
        equal "{\"tag\":\"NoShipperChoice\"}"
          (encodeJSON NoShipperChoice)
      test "Sum type, 1 arg constructor." do
        equal "{\"contents\":\"Test\",\"tag\":\"ShipperChoice\"}"
          (encodeJSON (ShipperChoice "Test"))
      testRoundTrip NoShipperChoice
      testRoundTrip (ShipperChoice "Test")
    suite "Nesting" do
      test "Nested no arg constructor" do
        equal "{\"tag\":\"FreightForwarderShipper\",\"contents\":{\"tag\":\"NoShipperChoice\"}}"
          (encodeJSON (FreightForwarderShipper NoShipperChoice))
      test "Nested 1 arg constructor" do
        equal "{\"tag\":\"FreightForwarderShipper\",\"contents\":{\"contents\":\"Test\",\"tag\":\"ShipperChoice\"}}"
          (encodeJSON (FreightForwarderShipper (ShipperChoice "Test")))
      testRoundTrip (FreightForwarderShipper NoShipperChoice)
      testRoundTrip (FreightForwarderShipper (ShipperChoice "Test"))

------------------------------------------------------------
opts :: Options
opts =
  defaultOptions
    { sumEncoding = aesonSumEncoding
    , unwrapSingleConstructors = true
    }

------------------------------------------------------------
newtype ARecord
  = ARecord
  { name :: String
  , age :: Int
  }

derive instance eqARecord :: Eq ARecord

derive instance genericARecord :: Generic ARecord _

instance showARecord :: Show ARecord where
  show = genericShow

instance decodeARecord :: Decode ARecord where
  decode value = genericDecode opts value

instance encodeARecord :: Encode ARecord where
  encode value = genericEncode opts value

data ShipperChoice
  = ShipperChoice String
  | NoShipperChoice

derive instance eqShipperChoice :: Eq ShipperChoice

derive instance genericShipperChoice :: Generic ShipperChoice _

instance showShipperChoice :: Show ShipperChoice where
  show = genericShow

instance decodeShipperChoice :: Decode ShipperChoice where
  decode value = genericDecode opts value

instance encodeShipperChoice :: Encode ShipperChoice where
  encode value = genericEncode opts value

data FreightForwarderChoice
  = FreightForwarderContact Int
  | FreightForwarderShipper ShipperChoice

derive instance eqFreightForwarderChoice :: Eq FreightForwarderChoice

derive instance genericFreightForwarderChoice :: Generic FreightForwarderChoice _

instance showFreightForwarderChoice :: Show FreightForwarderChoice where
  show = genericShow

instance decodeFreightForwarderChoice :: Decode FreightForwarderChoice where
  decode value = genericDecode opts value

instance encodeFreightForwarderChoice :: Encode FreightForwarderChoice where
  encode value = genericEncode opts value
