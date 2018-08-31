let main () =
  let session = 
    Notify.Session.create
      ~service_id:"27ba9f2a-2803-4c8e-82c1-1357de4b6a94"
      ~api_key:"447000ea-623b-48dc-928f-aef73346894a"
      ()
  in

  Lwt.(
    Notify.get_all_templates
      ~template_type:Notify.Email
      session
    >|= Printf.printf "wow. %s" >>= fun () ->
    Notify.get_template
      ~template_id:"a35b051c-98f3-403f-9e7b-d226bd22210f"
      session
    >|= Printf.printf "wow. %s" >>= fun () ->
    Notify.send_email_notification
      ~template_id:"1199828c-7a05-4c4a-b681-2781a6eaec28"
      ~email_address:"someone@example.com"
      session
    >|= Printf.printf "wow. %s" >>= fun () ->
    Notify.send_sms_notification
      ~template_id:"a35b051c-98f3-403f-9e7b-d226bd22210f"
      ~phone_number:"your-number-here"
      session
    >|= Printf.printf "wow. %s"
  )

let () =
  Lwt_main.run @@ main ()

