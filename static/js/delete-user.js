deleteUrl = '/delete-user';

function deleteUser(email, row, onSucc, onErr) {
	$.post( deleteUrl
		  , { user_email: email }
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

$('.delete-btn').click(function() {
    var hisTr = $(this).parent().parent();
    var email = $(hisTr).children("td").eq(0).text();

    deleteUser(email, hisTr, removeRow, showError);
});