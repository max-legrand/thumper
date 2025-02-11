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

let to_int = function
  | S100_Continue -> 100
  | S101_Switching_Protocols -> 101
  | S102_Processing -> 102
  | S200_OK -> 200
  | S201_Created -> 201
  | S202_Accepted -> 202
  | S203_Non_Authoritative_Information -> 203
  | S204_No_Content -> 204
  | S205_Reset_Content -> 205
  | S206_Partial_Content -> 206
  | S300_Multiple_Choices -> 300
  | S301_Moved_Permanently -> 301
  | S302_Found -> 302
  | S303_See_Other -> 303
  | S304_Not_Modified -> 304
  | S305_Use_Proxy -> 305
  | S307_Temporary_Redirect -> 307
  | S400_Bad_Request -> 400
  | S401_Unauthorized -> 401
  | S402_Payment_Required -> 402
  | S403_Forbidden -> 403
  | S404_Not_Found -> 404
  | S405_Method_Not_Allowed -> 405
  | S406_Not_Acceptable -> 406
  | S407_Proxy_Authentication_Required -> 407
  | S408_Request_Timeout -> 408
  | S409_Conflict -> 409
  | S410_Gone -> 410
  | S411_Length_Required -> 411
  | S412_Precondition_Failed -> 412
  | S413_Request_Entity_Too_Large -> 413
  | S414_Request_Uri_Too_Long -> 414
  | S415_Unsupported_Media_Type -> 415
  | S416_Requested_Range_Not_Satisfiable -> 416
  | S417_Expectation_Failed -> 417
  | S418_Im_A_Teapot -> 418
  | S429_Too_Many_Requests -> 429
  | S500_Internal_Server_Error -> 500
  | S501_Not_Implemented -> 501
  | S502_Bad_Gateway -> 502
  | S503_Service_Unavailable -> 503
  | S504_Gateway_Timeout -> 504
  | S505_Http_Version_Not_Supported -> 505
;;

let to_string = function
  | S100_Continue -> "100 Continue"
  | S101_Switching_Protocols -> "101 Switching Protocols"
  | S102_Processing -> "102 Processing"
  | S200_OK -> "200 OK"
  | S201_Created -> "201 Created"
  | S202_Accepted -> "202 Accepted"
  | S203_Non_Authoritative_Information -> "203 Non-Authoritative Information"
  | S204_No_Content -> "204 No Content"
  | S205_Reset_Content -> "205 Reset Content"
  | S206_Partial_Content -> "206 Partial Content"
  | S300_Multiple_Choices -> "300 Multiple Choices"
  | S301_Moved_Permanently -> "301 Moved Permanently"
  | S302_Found -> "302 Found"
  | S303_See_Other -> "303 See Other"
  | S304_Not_Modified -> "304 Not Modified"
  | S305_Use_Proxy -> "305 Use Proxy"
  | S307_Temporary_Redirect -> "307 Temporary Redirect"
  | S400_Bad_Request -> "400 Bad Request"
  | S401_Unauthorized -> "401 Unauthorized"
  | S402_Payment_Required -> "402 Payment Required"
  | S403_Forbidden -> "403 Forbidden"
  | S404_Not_Found -> "404 Not Found"
  | S405_Method_Not_Allowed -> "405 Method Not Allowed"
  | S406_Not_Acceptable -> "406 Not Acceptable"
  | S407_Proxy_Authentication_Required -> "407 Proxy Authentication Required"
  | S408_Request_Timeout -> "408 Request Timeout"
  | S409_Conflict -> "409 Conflict"
  | S410_Gone -> "410 Gone"
  | S411_Length_Required -> "411 Length Required"
  | S412_Precondition_Failed -> "412 Precondition Failed"
  | S413_Request_Entity_Too_Large -> "413 Request Entity Too Large"
  | S414_Request_Uri_Too_Long -> "414 Request-URI Too Long"
  | S415_Unsupported_Media_Type -> "415 Unsupported Media Type"
  | S416_Requested_Range_Not_Satisfiable -> "416 Requested Range Not Satisfiable"
  | S417_Expectation_Failed -> "417 Expectation Failed"
  | S418_Im_A_Teapot -> "418 I'm a teapot"
  | S429_Too_Many_Requests -> "429 Too Many Requests"
  | S500_Internal_Server_Error -> "500 Internal Server Error"
  | S501_Not_Implemented -> "501 Not Implemented"
  | S502_Bad_Gateway -> "502 Bad Gateway"
  | S503_Service_Unavailable -> "503 Service Unavailable"
  | S504_Gateway_Timeout -> "504 Gateway Timeout"
  | S505_Http_Version_Not_Supported -> "505 HTTP Version Not Supported"
;;

let of_int = function
  | 100 -> Some S100_Continue
  | 101 -> Some S101_Switching_Protocols
  | 102 -> Some S102_Processing
  | 200 -> Some S200_OK
  | 201 -> Some S201_Created
  | 202 -> Some S202_Accepted
  | 203 -> Some S203_Non_Authoritative_Information
  | 204 -> Some S204_No_Content
  | 205 -> Some S205_Reset_Content
  | 206 -> Some S206_Partial_Content
  | 300 -> Some S300_Multiple_Choices
  | 301 -> Some S301_Moved_Permanently
  | 302 -> Some S302_Found
  | 303 -> Some S303_See_Other
  | 304 -> Some S304_Not_Modified
  | 305 -> Some S305_Use_Proxy
  | 307 -> Some S307_Temporary_Redirect
  | 400 -> Some S400_Bad_Request
  | 401 -> Some S401_Unauthorized
  | 402 -> Some S402_Payment_Required
  | 403 -> Some S403_Forbidden
  | 404 -> Some S404_Not_Found
  | 405 -> Some S405_Method_Not_Allowed
  | 406 -> Some S406_Not_Acceptable
  | 407 -> Some S407_Proxy_Authentication_Required
  | 408 -> Some S408_Request_Timeout
  | 409 -> Some S409_Conflict
  | 410 -> Some S410_Gone
  | 411 -> Some S411_Length_Required
  | 412 -> Some S412_Precondition_Failed
  | 413 -> Some S413_Request_Entity_Too_Large
  | 414 -> Some S414_Request_Uri_Too_Long
  | 415 -> Some S415_Unsupported_Media_Type
  | 416 -> Some S416_Requested_Range_Not_Satisfiable
  | 417 -> Some S417_Expectation_Failed
  | 418 -> Some S418_Im_A_Teapot
  | 429 -> Some S429_Too_Many_Requests
  | 500 -> Some S500_Internal_Server_Error
  | 501 -> Some S501_Not_Implemented
  | 502 -> Some S502_Bad_Gateway
  | 503 -> Some S503_Service_Unavailable
  | 504 -> Some S504_Gateway_Timeout
  | 505 -> Some S505_Http_Version_Not_Supported
  | _ -> None
;;
