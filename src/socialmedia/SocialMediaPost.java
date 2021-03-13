package socialmedia;

import java.util.ArrayList;

public class SocialMediaPost {

  private static int count = 0;
  private int id;
  private SocialMediaAccount account;
  private String message;
  private boolean endorsed;
  private ArrayList<SocialMediaPost> endorsements = new ArrayList<>();
  private ArrayList<SocialMediaPost> comments = new ArrayList<>();
  private SocialMediaPost parent;

  public SocialMediaPost(SocialMediaAccount account, String message) throws InvalidPostException {
    if (message.isBlank() || message.length() > 100) {
      throw new InvalidPostException();
    }

    this.account = account;
    this.message = message;
    this.endorsed = message.startsWith("EP@");
    this.parent = null;
    this.id = ++count;
  }

  public SocialMediaPost(String message) {
    this.account = null;
    this.message = message;
    this.endorsed = message.startsWith("EP@");
    this.parent = null;
    this.id = ++count;
  }


  public int getId() {
    return id;
  }

  public SocialMediaAccount getAccount() {
    return account;
  }

  public String getMessage() {
    return message;
  }

  public Boolean getEndorsed() {
    return endorsed;
  }

  public ArrayList<SocialMediaPost> getEndorsements() {
    return endorsements;
  }

  public ArrayList<SocialMediaPost> getComments() {
    return comments;
  }

  public SocialMediaPost getParent() {
    return parent;
  }
  
  public void setId(int id) {
    this.id = id;
  }

  public void setAccount(SocialMediaAccount account) {
    this.account = account;
  }

  public void setMessage(String message) throws InvalidPostException {
    if (message.isBlank() || message.length() > 100) {
      throw new InvalidPostException();
    }
    
    this.message = message;
  }

  public void setEndorsed(Boolean endorsed) {
    this.endorsed = endorsed;
  }

  public void setEndorsements(ArrayList<SocialMediaPost> endorsements) {
    this.endorsements = endorsements;
  }

  public void setComments(ArrayList<SocialMediaPost> comments) {
    this.comments = comments;
  }

  public void setParent(SocialMediaPost parent) {
    this.parent = parent;
  }
}
