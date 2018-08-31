module Client = Cohttp_lwt_unix.Client

module Option = Monad.Option

module Dict = Map.Make(String)

let with_path uri ~path = Uri.with_path uri path

let get_body t = Lwt.(
    t >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body
  )

module Session = struct
  type t =
    { base_url : string
    ; service_id : string
    ; api_key : string
    }

  let create
      ?(base_url="http://localhost:6011")
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

  (*
  let post ?json ?params t ~path =
    let headers = headers t in
    let uri =
      Uri.of_string t.base_url
      |> with_path ~path
      |> fun default -> Option.(
          with_default ~default (params >>| Uri.with_query default)
        )
    and body = Option.(json >>| Yojson.to_string >>| Cohttp_lwt.Body.of_string)
    in
    Client.post ?body ~headers uri
     *)

  let get ?params t ~path =
    let headers = headers t in
    let uri = 
      Uri.of_string t.base_url
      |> with_path ~path
      |> fun default -> Option.(
          with_default ~default (params >>| Uri.with_query default)
        )
    in
    Client.get ~headers uri
end


type 'a dict = 'a Dict.t

type id = string

type sms_notification_response = string Lwt.t

type email_notification_response = string Lwt.t

type response = string Lwt.t

type template_type =
  | SMS
  | Email
  | Letter

let template_type_to_string = function
  | SMS -> "sms"
  | Email -> "email"
  | Letter -> "letter"

type status =
  | Delivered
  | Sending
  | Sent
  | Failed
  | TechnicalFailure

(*
let send_sms_notification
    ?personilization
    ?reference
    ?sms_sender_id
    session
    ~phone_number
    ~template_id =
  Lwt.return "WOW TJIS IS GREAT"

let send_email_notification
    ?personilization
    ?reference
    ?email_reply_to_id
    session
    ~email_address
    ~template_id =
  Lwt.return "damn yo"

let get_all_notifications
    ?template_type
    ?status
    ?reference
    ?older_than
    session =
  Lwt.return "HELLO NOTIFICATION"

let get_notification_by_id
    session
    ~notification_id =
  Lwt.return "HELLO NOTIFICATION"

let get_template 
    session
    ~template_id =
  Lwt.return "HELLO TEMPLATES"

let get_template_version
    session
    ~template_id
    ~version =
  Lwt.return "HELLO TEMPLATE VERSION"

let get_received_texts_number
    ?older_than
    session =
  Lwt.return "RECIEVED TEXETS"
  *)


let get_all_templates
    ?template_type
    session =
  get_body @@
  Session.get
    ~path:"/v2/templates"
    ?params:Option.(
        template_type >>| List.map template_type_to_string >>| fun t ->
        ["type", t]
      )
    session
