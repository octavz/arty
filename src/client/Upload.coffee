createUploader = ->
  uploader = new qq.FileUploader(
    element: document.getElementById("upl")
    allowedExtensions: ["jpg"]
    action: "/a/upload"
    debug: true
    onComplete: handleResponse
  )

handleResponse = (id, fname, json) ->
        obj = $("#preview")
        obj.attr "src", json.key
        $("#upl").css "display", "none"
        obj.css "display", "block"
        $('#fkey').val(json.key.split("/")[2])
        $('#frmItem').css "display", "block"

initPage = ->
        viewModel = new CmbModel(cmbData)
        ko.applyBindings viewModel

handleResponseItem = (data) ->
        console.log data
        alert if data.errCode is 0 then alert "OK" else alert dat


class CmbModel
  constructor: (data) ->
    @optMain = ko.observableArray(data.optMain)
    @optCats = ko.observableArray(data.optCats)
    @title = ko.observable('')
    @description = ko.observable('')
    @filter = ko.observable('')
    @category = ko.observable('')
    @guid = ko.observable('')

    @fcats = ko.computed =>
           f = @filter()
           if not f then [] else ko.utils.arrayFilter(@optCats(), (it) -> it.par is f.key)

    @saveItem = ->
              $.post "/a/postitem", $("#frm").serialize(), handleResponseItem
          

window.onload = ->
  createUploader()

$ ->
  initPage()

