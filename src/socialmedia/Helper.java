package socialmedia;
import java.util.ArrayList;

public class Helper {

  /**
   * This method is used to get an account using the user's handle
   * @param handle A string representing the user's handle
   * @param accounts An array list of social media accounts
   * @return The account
   */

  public static SocialMediaAccount getAccount(String handle,  ArrayList<SocialMediaAccount> accounts) {
    for (SocialMediaAccount currentAccount:accounts) {
      if (currentAccount.getHandle().equals(handle)) {
        return currentAccount;
      }
    }

    return null;
  }

  /**
   * This method is used to get an account using the user's id
   * @param id An integer representing the user's id
   * @param accounts An array list of social media accounts
   * @return The account
   */
  

  public static SocialMediaAccount getAccount(Integer id,  ArrayList<SocialMediaAccount> accounts) {
    for (SocialMediaAccount currentAccount:accounts) {
      if (currentAccount.getId() == id) {
        return currentAccount;
      }
    }

    return null;
  }

  /**
   * This method is used to get a post using the post's id
   * @param id An integer representing the post's id
   * @param accounts An array list of social media posts
   * @return The post
   */
  

  public static SocialMediaPost getPost(Integer id,  ArrayList<SocialMediaPost> posts) {
    for (SocialMediaPost currentPost:posts) {
      if (currentPost.getId() == id) {
        return currentPost;
      }
    }

    return null;
  }
}