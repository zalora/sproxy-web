var postUrl = '/domain/' + domain + '/privilege/' + privilege + '/rules';

makeEditable('.path-edit', pathUpdate, postUrl);
makeEditable('.method-edit', methodUpdate, postUrl);

function sendRule(pathField, methodField, onSucc, onErr, addRow) {
    $.post( postUrl
          , { operation: "add", path: pathField, method: methodField }
    ).done(function(data) {
        if(data == "added") {
            onSucc(pathField, methodField, addRow);
        }
        else {
            onErr(data);
        }
    }).fail(onErr);
}

function appendRow(pathField, methodField, addRow)
{
    fadeInOut("#updatesuccess");

    $('#edittable tbody').append(
        '<tr><td class="edit path-edit">' + pathField + '</td>' +
            '<td class="edit method-edit">' + methodField + '</td>' +
            '<td><a href="#" class="delete-btn btn btn-danger btn-xs">' + 
            '    Delete</a></td></tr>'); 

    $(addRow).children("td").eq(0).find('input').val("");
    $(addRow).children("td").eq(1).find('input').val("");

    makeEditable('.path-edit', pathUpdate, postUrl);
    makeEditable('.method-edit', methodUpdate, postUrl);
}

$('.add-btn').click(function() {
    var currRow    = $(this).parent().parent();
    var currPath   = $(currRow).children("td").eq(0).find('input').val();
    var currMethod = $(currRow).children("td").eq(1).find('input').val();

    if (currPath.length < 1 || currMethod.length < 1) {
        fadeInOut('#fieldempty');
    }
    else {
        sendRule(currPath, currMethod, appendRow, showError, currRow);
    }
});

function deleteRule(pathField, methodField, row, onSucc, onErr) {
    $.post( postUrl
          , { operation: "del", path: pathField, method: methodField }
    ).done(function(data) {
        if(data == "deleted") {
            onSucc(row);
        }
        else {
            onErr(data);
        }
    }).fail(onErr);
}

$('.delete-btn').click(function() {
    var hisTr = $(this).parent().parent();
    var path  = $(hisTr).children("td").eq(0).text();
    var method = $(hisTr).children("td").eq(1).text();

    deleteRule(path, method, hisTr, removeRow, showError);
});

function pathUpdate(newVal, prevVal, tr) {
    var currPath   = newVal;
    var currMethod = $(tr).children("td").eq(1).text();

    var obj = { operation: "upd"
              , what: "path"
              , newpath: currPath 
              , oldpath: prevVal
              , method: currMethod
              };

    return obj;
}

function methodUpdate(newVal, prevVal, tr) {
    var currMethod = newVal;
    var currPath   = $(tr).children("td").eq(0).text();

    var obj = { operation: "upd" 
              , what:      "method" 
              , newmethod: currMethod
              , oldmethod: prevVal
              , path:      currPath 
              };
    return obj;
}
