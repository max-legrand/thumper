(** HTTP status codes *)
type status_code =
  | S100_Continue
  | S101_Switching_Protocols
  | S102_Processing
  | S200_OK
  | S201_Created
  | S202_Accepted
  | S203_Non_Authoritative_Information
  | S204_No_Content
  | S205_Reset_Content
  | S206_Partial_Content
  | S300_Multiple_Choices
  | S301_Moved_Permanently
  | S302_Found
  | S303_See_Other
  | S304_Not_Modified
  | S305_Use_Proxy
  | S307_Temporary_Redirect
  | S400_Bad_Request
  | S401_Unauthorized
  | S402_Payment_Required
  | S403_Forbidden
  | S404_Not_Found
  | S405_Method_Not_Allowed
  | S406_Not_Acceptable
  | S407_Proxy_Authentication_Required
  | S408_Request_Timeout
  | S409_Conflict
  | S410_Gone
  | S411_Length_Required
  | S412_Precondition_Failed
  | S413_Request_Entity_Too_Large
  | S414_Request_Uri_Too_Long
  | S415_Unsupported_Media_Type
  | S416_Requested_Range_Not_Satisfiable
  | S417_Expectation_Failed
  | S418_Im_A_Teapot
  | S429_Too_Many_Requests
  | S500_Internal_Server_Error
  | S501_Not_Implemented
  | S502_Bad_Gateway
  | S503_Service_Unavailable
  | S504_Gateway_Timeout
  | S505_Http_Version_Not_Supported
[@@deriving show, enum, sexp]

(** Convert a status code to its corresponding integer value. *)
val to_int : status_code -> int

(** Convert a status code to its corresponding string representation. *)
val to_string : status_code -> string

(** Convert an integer to its corresponding status code, if it exists. *)
val of_int : int -> status_code option
