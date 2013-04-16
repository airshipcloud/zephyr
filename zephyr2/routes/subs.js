
/*
 * GET home page.
 */

exports.subs = function(req, res){
  var id = req.params[0];
  console.log(id);
  res.render('subs', { title: 'Express' });
};