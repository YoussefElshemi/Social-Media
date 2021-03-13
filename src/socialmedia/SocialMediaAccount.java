package socialmedia;

import java.util.ArrayList;

public class SocialMediaAccount {
  
  private static int count = 0;
  private int id;
  private String handle;
  private ArrayList<SocialMediaPost> posts;

  public SocialMediaAccount(String handle) throws InvalidHandleException {
    if (handle.length() > 30 || handle.isBlank() || handle.contains(" ")) {
      throw new InvalidHandleException();
    }

    this.handle = handle;
    this.id = ++count;
  }

  public int getId() {
    return id;
  }

  public String getHandle() {
    return handle;
  }

  public ArrayList<SocialMediaPost> getPosts() {
    return posts;
  }

  public void setId(int id) {
    this.id = id;
  }

  public void setHandle(String handle) throws InvalidHandleException {
    if (handle.length() > 30 || handle.isBlank() || handle.contains(" ")) {
      throw new InvalidHandleException();
    }
    this.handle = handle;
  }

  public void setPosts(ArrayList<SocialMediaPost> posts) {
    this.posts = posts;
  }
}
