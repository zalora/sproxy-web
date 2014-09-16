postUrl = '/group/' + groupName + '/members';

makeEditable('.member-edit', memberUpdate, postUrl);

function memberUpdate(newVal, prevVal, tr)
{
    var obj = { operation: 'upd' 
              , new: newVal 
              , old: prevVal 
              };

    return obj;
}

$('.add-btn').click(function() {
    var currRow    = $(this).parent().parent();
    var currMember = $(currRow).children("td").eq(0).find('input').val(); 

    if (currMember.length < 1) {
        fadeInOut("#fieldempty");
    }
    else {
        sendMember(currMember, appendRow, showError, currRow);
    }
});

$('.delete-btn').click(function() {
    var hisTr  = $(this).parent().parent();
    var member = $(hisTr).children("td").eq(0).text();

    deleteMember(member, hisTr, removeRow, showError);
});

function sendMember(memberField, onSucc, onErr, addRow) {
    $.post( postUrl 
          , { operation: "add", member: memberField } 
    ).done(function(data) {
        onSucc(memberField, addRow);
    }).fail(onErr);
}

function deleteMember(memberField, row, onSucc, onErr) {
    $.post( postUrl 
          , { operation: "del", member: memberField }
    ).done(function(data) {
            onSucc(row);
    }).fail(onErr);
}

function appendRow(memberField, addRow) {
    fadeInOut("#updatesuccess");

    $('#edittable tbody').append(
        '<tr><td class="edit member-edit">' + memberField + '</td>' + 
        '<td><a href="#" class="delete-btn btn btn-danger btn-xs">Delete</a></td></tr>');

    $(addRow).children("td").eq(0).find('input').val("");

    makeEditable('.member-edit', memberUpdate, postUrl);
    
    $('.delete-btn').click(function() {
        var hisTr  = $(this).parent().parent();
        var member = $(hisTr).children("td").eq(0).text();

        deleteMember(member, hisTr, removeRow, showError);
    });
}
