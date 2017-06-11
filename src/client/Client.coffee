$ = jQuery
# window.showLogIn = (elem) -> $(elem).w2overlay($("#popup1 [rel=body]").html(), { css: { width: "600px", padding: "10px" } })

createCookie = (name, value, days) ->
  if days
    date = new Date()
    date.setTime date.getTime() + (days * 24 * 60 * 60 * 1000)
    expires = "; expires=" + date.toGMTString()
  else
    expires = ""
  document.cookie = name + "=" + value + expires + "; path=/"

getCookie = (name) ->
  nameEQ = name + "="
  ca = document.cookie.split(";")
  i = 0

  while i < ca.length
    c = ca[i]
    c = c.substring(1, c.length)  while c.charAt(0) is " "
    return c.substring(nameEQ.length, c.length)  if c.indexOf(nameEQ) is 0
    i++
  null

eraseCookie = (name) ->
  createCookie name, "", -1

window.mainForm = -> $("#theForm")

post = (url, postData, successHandler) ->
    $.ajax
            url: url,
            type: "POST"
            data: postData
            error: (res) -> alert(res)
            success: (data) ->
                res = data.er
                switch res.c
                    when 0 then successHandler data
                    else alert res.m

getSid = -> getCookie("art_sid")

redirect = uri -> window.location.href = uri

# test asdasdsdadsdada
window.logIn = (frm) ->
    post "/a/login", frm.serialize(),
    (data) ->
        createCookie "art_sid", data.d.s, 1
        createCookie "art_em", data.d.e, 1
        redirect "/"

window.logOut = ->
    post "/a/logout", "s=" + getSid(),
    (data) ->
        eraseCookie "art_sid"
        eraseCookie "art_em"
        redirect "/"
           
