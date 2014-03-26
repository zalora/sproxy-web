postUrl = '/domains';

makeEditable('.domain-edit', domainUpdate, postUrl);

function domainUpdate(newVal, prevVal, tr) {
    var obj = { operation: 'upd'
              , new: newVal 
              , old: prevVal 
              };

    return obj;
}


$('.add-btn').click(function() {
    var currRow    = $(this).parent().parent();
    var currDomain = $(currRow).children("td").eq(0).find('input').val();

    if (currDomain.length < 1) {
        fadeInOut('#fieldempty');
    }
    else {
        sendDomain(currDomain, appendRow, showError, currRow);
    }
});

$('.delete-btn').click(function() {
    var hisTr  = $(this).parent().parent();
    var domain = $(hisTr).children("td").eq(0).text();

    deleteDomain(domain, hisTr, removeRow, showError);
});

$('.privileges-btn').click(function() {
    var hisTr = $(this).parent().parent();
    var domain = $(hisTr).children("td").eq(0).text();

    window.location = '/domain/' + domain + '/privileges';
});

function sendDomain(domainField, onSucc, onErr, addRow) {
    $.post( postUrl 
          , { operation: "add", domain: domainField } 
    ).done(function(data) {
        if(data == "added") {
            onSucc(domainField, addRow);
        }
        else {
            onErr(data);
        }
    }).fail(onErr);
}

function deleteDomain(domainField, row, onSucc, onErr) {
    $.post( postUrl 
          , { operation: "del", domain: domainField }
    ).done(function(data) {
        if(data == "deleted")
        {
            onSucc(row);
        }
        else {
            onErr(data);
        }
    }).fail(onErr);
}

function appendRow(domainField, addRow)
{
    fadeInOut("#updatesuccess");

    $('#edittable tbody').append(
        '<tr><td class="edit domain-edit">' + domainField + '</td>' +
        '<td><a href="#" class="delete-btn btn btn-danger btn-xs">Delete</a></td>' + 
        '<td><a href="#" class="privilege-btn btn btn-link btn-xs">Privileges</a></td></tr>');

    $(addRow).children("td").eq(0).find('input').val("");

    makeEditable('.domain-edit', domainUpdate, postUrl);

    $('.delete-btn').click(function() {
        var hisTr  = $(this).parent().parent();
        var domain = $(hisTr).children("td").eq(0).text();

        deleteDomain(domain, hisTr, removeRow, showError);
    });

    $('.privilege-btn').click(function() {
        var hisTr  = $(this).parent().parent();
        var domain = $(hisTr).children("td").eq(0).text();

        window.location = '/domain/' + domain + '/privileges';
    });
}
