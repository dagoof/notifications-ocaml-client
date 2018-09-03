module Session : sig
  type t
  val create :
    ?base_url:string ->
    api_key:string ->
    service_id:string ->
    unit ->
    t

  val create_jwt : t -> string
end

module Dict : sig
  include Map.S with type key = string

  val of_list : (key * 'a) list -> 'a t
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

type status =
  | Sending
  | Delivered
  | PermanentFailure
  | TemporaryFailure
  | TechnicalFailure

val send_email_notification :
  ?personalisation:string dict ->
  ?reference:id ->
  ?email_reply_to_id:string ->
  Session.t ->
  email_address:string ->
  template_id:id ->
  email_notification_response

val send_sms_notification :
  ?personalisation:string dict ->
  ?reference:id ->
  ?sms_sender_id:id ->
  Session.t ->
  phone_number:string ->
  template_id:id ->
  sms_notification_response

val get_notification_by_id :
  Session.t ->
  notification_id:id ->
  response

val get_all_notifications :
  ?template_type:template_type ->
  ?status:status ->
  ?reference:id ->
  ?older_than:id ->
  Session.t ->
  response

val get_template_version :
  Session.t ->
  template_id:id ->
  version:int ->
  response

val get_received_texts_number :
  ?older_than:id ->
  Session.t ->
  response

val get_template :
  Session.t ->
  template_id:id ->
  response

val get_all_templates :
  ?template_type:template_type ->
  Session.t ->
  response

