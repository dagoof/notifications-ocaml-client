let service_id  = Sys.getenv "SERVICE_ID"
let api_key     = Sys.getenv "API_KEY"
let template_id = Sys.getenv "TEMPLATE_ID"
let base_url    = Sys.getenv_opt "API_BASE_URL"
let phone_number = Sys.getenv "PHONE_NUMBER"

let main () =
  let session = 
    Notify.Session.create
      ?base_url
      ~service_id
      ~api_key
      ()
  in

  Lwt.(
    Notify.get_all_templates
      ~template_type:Notify.SMS
      session
    >|= Printf.printf "%s" >>= fun () ->
    Notify.send_sms_notification
      ~template_id
      ~phone_number
      ~reference:"notify-ocaml-ref-123"
      ~personalisation:Notify.Dict.(of_list [
          "reason", "this is forever town";
        ])
      session
    >|= Printf.printf "%s"
  )

let () =
  Lwt_main.run @@ Lwt_list.iter_p main [()]
