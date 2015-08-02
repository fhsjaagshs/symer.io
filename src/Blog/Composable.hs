module Blog.Composable where
  
import Blog.User
import Text.Blaze.Html5 (Html)

-- renders records into HTML using blaze
class Composable a where
  render :: a -> Maybe User -> Html
  renderBasic :: a -> Html
  renderBasic comp = render comp Nothing