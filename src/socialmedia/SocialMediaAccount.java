package socialmedia;

import java.io.Serializable;
import java.util.ArrayList;

public class SocialMediaAccount implements Serializable {
  
  private static final long serialVersionUID = 3826223542991329844L;
  private static int count = 1;
  private int id;
  private String handle;
  private String description;
  private ArrayList<SocialMediaPost> posts =  new ArrayList<>();
  private ArrayList<SocialMediaPost> endorsedPosts =  new ArrayList<>();

  public SocialMediaAccount(String handle) throws InvalidHandleException {
    if (handle.length() > 30 || handle.isBlank() || handle.contains(" ")) {
      throw new InvalidHandleException();
    }

    this.handle = handle;
    this.description = null;
    this.id = count++;
  }

  public SocialMediaAccount(String handle, String description) throws InvalidHandleException {
    if (handle.length() > 30 || handle.isBlank() || handle.contains(" ")) {
      throw new InvalidHandleException();
    }

    this.handle = handle;
    this.description = description;
    this.id = count++;
  }



  public int getId() {
    return id;
  }

  public String getHandle() {
    return handle;
  }

  public String getDescription() {
    return description;
  }

  public ArrayList<SocialMediaPost> getPosts() {
    return posts;
  }

  public ArrayList<SocialMediaPost> getEndorsedPosts() {
    return endorsedPosts;
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

  public void setDescription(String description) {
    this.description = description;
  }

  public void setPosts(ArrayList<SocialMediaPost> posts) {
    this.posts = posts;
  }

  public void setEndorsedPosts(ArrayList<SocialMediaPost> endorsedPosts) {
    this.endorsedPosts = endorsedPosts;
  }
}
