module Client = Cohttp_lwt_unix.Client

module Option = Monad.Option

module Dict = Map.Make(String)

(*
let pairs dict =
  Dict.to_seq dict |> List.of_seq

let filter_map f lst =
  List.map f lst
  |> List.filter (fun item ->
      Option.( with_default ~default:false @@ item >>| fun _ -> true)
    )
*)

let with_path uri ~path = Uri.with_path uri path

let get_body t = Lwt.(
    t >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body
  )

module Json = struct
  type t = Yojson.json Dict.t

  let empty = Dict.empty

  let add key ?value t =
    Option.(
      with_default ~default:t 
        (value >>| fun v -> Dict.add key v t)
    )

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

  let post ?json ?params t ~path =
    let headers = headers t in
    let uri =
      Uri.of_string t.base_url
      |> with_path ~path
      |> fun default -> Option.(
          with_default ~default (params >>| Uri.with_query default)
        )
    and body = Option.map Json.to_cohttp_body json
    in
    Client.post ?body ~headers uri

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

let send_email_notification
    ?personalisation
    ?reference
    ?email_reply_to_id
    session
    ~email_address
    ~template_id =
  get_body @@
  Session.post
    ~path:"/v2/notifications/email"
    ~json:Json.(
        empty
        |> add "email_address" ~value:(`String email_address)
        |> add "template_id" ~value:(`String template_id)
        |> add "reference" ?value:Option.(reference >>| fun ref -> `String ref)
      )
    session

let send_sms_notification
    ?personalisation
    ?reference
    ?sms_sender_id
    session
    ~phone_number
    ~template_id =
  get_body @@
  Session.post
    ~path:"/v2/notifications/sms"
    ~json:Json.(
        empty
        |> add "phone_number" ~value:(`String phone_number)
        |> add "template_id" ~value:(`String template_id)
        |> add "reference" ?value:Option.(reference >>| fun ref -> `String ref)
      )
    session

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

let get_template_version
    session
    ~template_id
    ~version =
  Lwt.return "HELLO TEMPLATE VERSION"

let get_received_texts_number
    ?older_than
    session =
  Lwt.return "RECIEVED TEXETS"

(*
        notification = {
            "email_address": email_address,
            "template_id": template_id
        }
        if personalisation:
            personalisation = personalisation.copy()
            for key in personalisation:
                if isinstance(personalisation[key], io.IOBase):
                    personalisation[key] = {
                        'file': base64.b64encode(personalisation[key].read()).decode('ascii')
                    }
            notification.update({'personalisation': personalisation})
        if reference:
            notification.update({'reference': reference})
        if email_reply_to_id:
            notification.update({'email_reply_to_id': email_reply_to_id})
        return self.post(
            '/v2/notifications/email',
            data=notification)
   *)


let get_all_templates
    ?template_type
    session =
  get_body @@
  Session.get
    ~path:"/v2/templates"
    ?params:Option.(
        template_type >>| template_type_to_string >>| fun t ->
        ["type", [t]]
      )
    session

let get_template 
    session
    ~template_id =
  get_body @@
  Session.get
    ~path:Uri.("/v2/template/" ^ pct_encode template_id)
    session
