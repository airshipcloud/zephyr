
/*
 * GET home page.
 */

exports.subs = function(req, res){
  var id = req.params[0];
  res.render('subs', { title: 'Express' });
};