postUrl = '/groups';

makeEditable('.group-edit', groupUpdate, postUrl);

function groupUpdate(newVal, prevVal, tr) {
    var obj = { operation: 'upd'
              , new: newVal 
              , old: prevVal 
              };

    return obj;
}


$('.add-btn').click(function() {
    var currRow   = $(this).parent().parent();
    var currGroup = $(currRow).children("td").eq(0).find('input').val();

    if (currGroup.length < 1) {
        fadeInOut('#fieldempty');
    }
    else {
        sendGroup(currGroup, appendRow, showError, currRow);
    }
});

$('.delete-btn').click(function() {
    var hisTr = $(this).parent().parent();
    var group = $(hisTr).children("td").eq(0).text();

    deleteGroup(group, hisTr, removeRow, showError);
});

$('.member-btn').click(function() {
    var hisTr = $(this).parent().parent();
    var group = $(hisTr).children("td").eq(0).text();

    window.location = '/group/' + group + '/members';
});

function sendGroup(groupField, onSucc, onErr, addRow) {
    $.post( postUrl 
          , { operation: "add", group: groupField } 
    ).done(function(data) {
        onSucc(groupField, addRow);
    }).fail(onErr);
}

function deleteGroup(groupField, row, onSucc, onErr) {
    $.post( postUrl 
          , { operation: "del", group: groupField }
    ).done(function(data) {
        onSucc(row);
    }).fail(onErr);
}

function appendRow(groupField, addRow)
{
    fadeInOut("#updatesuccess");

    $('#edittable tbody').append(
        '<tr><td class="edit group-edit">' + groupField + '</td>' +
        '<td><a href="#" class="delete-btn btn btn-danger btn-xs">Delete</a></td>' + 
        '<td><a href="#" class="member-btn btn btn-link btn-xs">Members</a></td></tr>');

    $(addRow).children("td").eq(0).find('input').val("");

    makeEditable('.group-edit', groupUpdate, postUrl);

    $('.delete-btn').click(function() {
        var hisTr = $(this).parent().parent();
        var group = $(hisTr).children("td").eq(0).text();

        deleteGroup(group, hisTr, removeRow, showError);
    });

    $('.member-btn').click(function() {
        var hisTr = $(this).parent().parent();
        var group = $(hisTr).children("td").eq(0).text();

        window.location = '/group/' + group + '/members';
    });
}
