let (>>>) f g x = g @@ f x

module Client = Cohttp_lwt_unix.Client

module Option = Monad.Option

module Dict = struct
  include Map.Make(String)

  let of_list l = List.to_seq l |> of_seq
end

let with_path uri ~path = Uri.with_path uri path

let get_body t = Lwt.(
    t >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body
  )

module Params = struct
  module StringSet = Set.Make(String)

  type t = StringSet.t Dict.t

  let empty = Dict.empty

  let create_set ?value () =
    Option.(
      with_default
        ~default:StringSet.empty
        (value >>| StringSet.singleton)
    )

  let add key ?value t =
    let combine merge a b =
      Option.(
        in_absence ?sometimes:b @@
        in_absence ?sometimes:a @@
        map2 merge a b
      )
    in

    Dict.update key (fun existing ->
        combine
          StringSet.union
          Option.(value >>| StringSet.singleton)
          existing
      )
      t

  let without ~key t =
    Dict.remove key t

  let to_query_params t =
    t
    |> Dict.map (StringSet.to_seq >>> List.of_seq)
    |> Dict.to_seq
    |> List.of_seq

  let set_query ?params uri =
    Option.(
      with_default
        ~default:uri
        (params >>| to_query_params >>| Uri.with_query uri)
    )
end

module Json = struct
  type t = Yojson.t Dict.t

  let empty = Dict.empty

  let string s = `String s

  let assoc o = `Assoc o

  let add key ?value t =
    Option.(
      with_default ~default:t 
        (value >>| fun v -> Dict.add key v t)
    )

  let add_s key ?value t =
    add key ?value:Option.(value >>| string) t

  let without ~key t =
    Dict.remove key t

  let to_json t =
    `Assoc (Dict.to_seq t |> List.of_seq)

  let to_string t =
    Yojson.to_string @@ to_json t

  let to_cohttp_body t =
    Cohttp_lwt.Body.of_string @@ to_string t
end

module Session = struct
  type t =
    { base_url : string
    ; service_id : string
    ; api_key : string
    }


  let create
      ?(base_url="https://rest-api.notify.gov.au")
      ~api_key
      ~service_id () =
    { base_url; service_id; api_key }

  let create_jwt {service_id; api_key; _} =
    let now = string_of_int @@ int_of_float @@ Unix.time () in
    let header = Jwt.(header_of_algorithm_and_typ (HS256 api_key) "JWT")
    and payload = Jwt.(
        empty_payload
        |> add_claim iss service_id
        |> add_claim iat now
      )
    in
    Jwt.(
      t_of_header_and_payload header payload |> token_of_t
    )

  let headers t =
    Cohttp.Header.of_list
      [ "Content-type", "application/json"
      ; "Authorization",  Printf.sprintf "Bearer %s" @@ create_jwt t
      ; "User-agent", "NOTIFY-API-OCAML-CLIENT/0.1"
      ]

  let post ?json ?params t ~path =
    let headers = headers t in
    let uri =
      Uri.of_string t.base_url
      |> with_path ~path
      |> Params.set_query ?params
    and body = Option.map Json.to_cohttp_body json
    in
    Client.post ?body ~chunked:false ~headers uri

  let get ?params t ~path =
    let headers = headers t in
    let uri = 
      Uri.of_string t.base_url
      |> with_path ~path
      |> Params.set_query ?params
    in
    Printf.printf "%s\n" (Uri.to_string uri);
    Client.get ~headers uri
end


type 'a dict = 'a Dict.t

type id = string

type sms_notification_response = string Lwt.t

type email_notification_response = string Lwt.t

type response = string Lwt.t

let personalise = Json.(Dict.map string >>> to_json)

type template_type =
  | SMS
  | Email
  | Letter

let tt_to_s = function
  | SMS -> "sms"
  | Email -> "email"
  | Letter -> "letter"

type status =
  | Sending
  | Delivered
  | PermanentFailure
  | TemporaryFailure
  | TechnicalFailure

let status_to_s = function
  | Sending -> "sending"
  | Delivered -> "delivered"
  | PermanentFailure -> "permanent-failure"
  | TemporaryFailure -> "temporary-failure"
  | TechnicalFailure -> "technical-failure"

let slash_join f s = String.concat " / " @@ List.map f s

let statuses_to_s = slash_join status_to_s

let tts_to_s = slash_join tt_to_s

let send_email_notification
    ?personalisation
    ?reference
    ?status_callback_url
    ?status_callback_bearer_token
    ?email_reply_to_id
    session
    ~email_address
    ~template_id =
  get_body @@
  Session.post
    ~path:"/v2/notifications/email"
    ~json:Json.(
        empty
        |> add_s "email_address" ~value:email_address
        |> add_s "template_id" ~value:template_id
        |> add_s "reference" ?value:reference
        |> add_s "email_reply_to_id" ?value:email_reply_to_id
        |> add_s "status_callback_url" ?value:status_callback_url
        |> add_s "status_callback_bearer_token" ?value:status_callback_bearer_token
        |> add "personalisation" ?value:Option.(personalisation >>| personalise)
      )
    session

let send_sms_notification
    ?personalisation
    ?reference
    ?status_callback_url
    ?status_callback_bearer_token
    ?sms_sender_id
    session
    ~phone_number
    ~template_id =
  get_body @@
  Session.post
    ~path:"/v2/notifications/sms"
    ~json:Json.(
        empty
        |> add_s "phone_number" ~value:phone_number
        |> add_s "template_id" ~value:template_id
        |> add_s "reference" ?value:reference
        |> add_s "sms_sender_id" ?value:sms_sender_id
        |> add_s "status_callback_url" ?value:status_callback_url
        |> add_s "status_callback_bearer_token" ?value:status_callback_bearer_token
        |> add "personalisation" ?value:Option.(personalisation >>| personalise)
      )
    session

let get_all_notifications
    ?template_type
    ?status
    ?reference
    ?older_than
    session =
  get_body @@
  Session.get
    ~path:"/v2/notifications"
    ~params:Params.(
        empty
        |> add "status" ?value:Option.(status >>| status_to_s)
        |> add "template_type" ?value:Option.(template_type >>| tt_to_s)
        |> add "reference" ?value:reference
        |> add "older_than" ?value:older_than
      )
    session

let get_notification_by_id
    session
    ~notification_id =
  get_body @@
  Session.get
    ~path:Uri.("/v2/notifications/" ^ pct_encode notification_id)
    session

let get_template_version
    session
    ~template_id
    ~version =
  get_body @@
  Session.get
    ~path:Uri.(
        Printf.sprintf "/v2/template/%s/version/%s"
          (pct_encode template_id)
          (pct_encode @@ string_of_int version)
      )
    session

let get_received_texts_number
    ?older_than
    session =
  get_body @@
  Session.get
    ~path:"/v2/received-text-messages"
    ~params:Params.(
        empty
        |> add "older_than" ?value:older_than
      )
    session

let get_all_templates
    ?template_type
    session =
  get_body @@
  Session.get
    ~path:"/v2/templates"
    ~params:Params.(
        empty
        |> add "type" ?value:Option.(template_type >>| tt_to_s)
      )
    session

let get_template 
    session
    ~template_id =
  get_body @@
  Session.get
    ~path:Uri.("/v2/template/" ^ pct_encode template_id)
    session
